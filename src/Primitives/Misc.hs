{-# LANGUAGE LambdaCase #-}
module Primitives.Misc (primitives, macros) where

-- TODO split into Control and Syntax

import Text.Read (readMaybe)

import Parsers (load)
import Val
import Evaluation
import EvaluationMonad
import Primitives.String (stringSH, unwrapStringPH)
-- r7rs errors probably don't need this
import Primitives.WriteLib (writeSharedSH)
import Control.Monad.Reader -- for qq

-- TODO NEXT: this module is broken right now because of Pairs

primitives :: [Primitive]
primitives = [ identityFunction
             , evalPrim
             , applyFunc
             , errorFunc
             , quit
             , loadPrim
             , callWithCurrentContinuation
             ]

macros :: [(String, Macro)]
macros = [ ("quote", quote)
         , ("quasiquote", quasiquote)
         , ("if", ifMacro)
         , ("set!", set)
         , ("set-option!", setOpt)
         , ("define", define)
         , ("lambda", lambda)
         , ("defmacro", defmacro)
         , ("begin", begin)
         ]

identityFunction :: Primitive
identityFunction = Prim "id" (Exactly 1) $ \[arg] -> return arg

callWithCurrentContinuation :: Primitive
callWithCurrentContinuation = 
  Prim "call-with-current-continuation" (Exactly 1) callcc
  where
    callcc :: [Val] -> EM Val
    callcc [func] = callCC $ \k ->
        tailCall func [Continuation k]

-- compose?

quote :: Macro
quote = Macro (Exactly 1) $ \_ [form]  -> return form

ifMacro :: Macro
ifMacro = Macro (AtLeast 2) $ 
  \tail (pred : conseq : alts) -> do
    p <- evalBody pred
    if truthy p
    then eval tail conseq
    else case alts of
      [] -> return Nil -- r7rs says unspecified
      xs -> evalSeq tail xs

set :: Macro
set = Macro (Exactly 2) $ \_ -> \case
  [Symbol var, form] -> evalBody form >>= setVar var
  [notAtom, _]     -> throwError $ TypeMismatch "symbol" notAtom

setOpt :: Macro
setOpt = Macro (Exactly 2) $ \_ -> \case
  [Symbol optName, form] -> do
    val <- evalBody form
    let mopt = readMaybe optName
    case mopt of
      Nothing -> pure ()
      Just opt -> if truthy val then enableOpt opt else disableOpt opt
    return val
  [notAtom, _] -> throwError $ TypeMismatch "symbol" notAtom

defineBuiltin :: Builtin
-- arity is AtLeast 1 so can't be called with no args
defineBuiltin (x:xs) = do
  defnHead <- freezeList x
  defineBuiltinFrozen defnHead xs

  where
    defineBuiltinFrozen :: FrozenList -> [Val] -> EM Val
    defineBuiltinFrozen (FNotList (Symbol var)) [form] = do
      setVarForCapture var
      val <- evalBody form
      let renamed = case val of
            Closure{} -> val { name = Just var }
            _ -> val
      defineVar var renamed
    defineBuiltinFrozen (FNotList (Symbol var)) badForms =
      throwError $ NumArgs (Exactly 2) (Symbol var : badForms)
    
    defineBuiltinFrozen (FList (Symbol name : params)) body = case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body (Just name) >>= defineVar name
    
    defineBuiltinFrozen (FDottedList (Symbol name : params) varargs) body = 
      case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name
    
    defineBuiltinFrozen (FList (notAtom : _)) _ = 
      throwError $ TypeMismatch "symbol" notAtom
    
    defineBuiltinFrozen (FDottedList (notAtom : _) _) _ =
      throwError $ TypeMismatch "symbol" notAtom
    
    defineBuiltinFrozen _notAtomOrList _ = 
      throwError $ TypeMismatch "symbol or list" x

-- arities are AtLeast 1 so that we can throw a nicer error than just
-- "expects at least 2 arguments" in the case of defining a function.
-- 'define's builtin throws the regular Exactly 2 error if it isn't defining
-- a function.
define, lambda :: Macro
define = Macro (AtLeast 1) $ const defineBuiltin
lambda = Macro (AtLeast 1) $ const mkLambda

mkLambda :: [Val] -> EM Val
-- arity is AtLeast 1 so can't be called with no args
mkLambda (formals : body) = do 
  frozenFormals <- freezeList formals
  case frozenFormals of
    FList params -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body Nothing
    FDottedList params varargs -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs params body Nothing
    FNotList varargs@(Symbol _) -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs [] body Nothing
    _notAtomOrList -> throwError $ TypeMismatch "symbol or list" formals

emptyBodyError :: LispErr
emptyBodyError = EmptyBody -- historical artifact, feel free to inline

makeFunc :: Maybe String
         -> [Val]
         -> [Val]
         -> Maybe String
         -> EM Val
makeFunc varargs params body name = do
    maybe (pure ()) setVarForCapture name
    env <- envSnapshot
    return $ Closure (map show params) varargs body env name

makeFuncNormal :: [Val] -> [Val] -> Maybe String -> EM Val
makeFuncNormal = makeFunc Nothing
makeFuncVarargs :: Val -> [Val] -> [Val] -> Maybe String -> EM Val
makeFuncVarargs = makeFunc . Just . show

defmacro :: Macro
defmacro = Macro (AtLeast 2) $ const $
  \(Symbol name : ps : body) -> do
    lam <- mkLambda (ps : body)
    macro <- makeImproperMutableList [Symbol "macro"] lam
    defineVar name macro

begin :: Macro
begin = Macro (AtLeast 1) $ \tail stmts -> evalSeq tail stmts

evalPrim :: Primitive
evalPrim = Prim "eval" (Exactly 1) $ \[form] -> evalTail form

-- TODO: [r7rs]
applyFunc :: Primitive
applyFunc = Prim "apply" (AtLeast 1) $ \case
  [func, form] -> do
    frozenForm <- freezeList form
    case frozenForm of
      FList args -> tailCall func args
      _bad -> throwError $ TypeMismatch "list" form
  (func : args) -> tailCall func args

loadPrim :: Primitive
loadPrim = Prim "load" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          file <- liftIO . runExceptT $ load filename
          case file of
            Left e   -> throwError e
            Right ls -> do
              state <- get
              -- put (interaction-environment)
              put $ ES (globalEnv state) [] (options state)
              r <- evalBodySeq ls
              put state
              return r
        | otherwise -> throwError $ TypeMismatch "string" val

quasiquote :: Macro
quasiquote = Macro (Exactly 1) $ \_ [form]-> runReaderT (qq form) 0
  where 
    qq :: Val -> ReaderT Int EM Val
    qq term = lift (freezeList term) >>= \case
      FList [Symbol "quasiquote", form] -> local (+ 1) $ do
        inner <- qq form
        lift $ makeMutableList [Symbol "quasiquote", inner]
      FList [Symbol "unquote", form] -> do
        depth <- ask
        if depth == 0
        then lift $ evalBody form
        else local (subtract 1) $ do
          inner <- qq form
          lift $ makeMutableList [Symbol "unquote", inner]
      FList forms
          -- this can happen because the parser is smart enough to rewrite
          -- `(0 . ,lst) as IList [Number 0, Symbol unquote, Symbol lst]
          -- however whenever this happens, ',lst' will definitely be of size 2
        | Symbol "unquote" `elem` forms
        , (list, dot) <- break (== Symbol "unquote") forms
        , [_,_] <- dot
        -> do lift (makeMutableList dot) >>= qqImproper list
        | otherwise
        -> qqTerms forms >>= lift . makeMutableList
      FDottedList forms form -> qqImproper forms form
      _other -> return term

    qqTerms :: [Val] -> ReaderT Int EM [Val]
    qqTerms [] = return []
    qqTerms (t:ts) = do
      frozenT <- lift $ freezeList t
      terms <- case frozenT of
        FList [Symbol "unquote-splicing", form] -> do
          depth <- ask
          val <- if depth == 0
                 then lift $ evalBody form
                 else local (subtract 1) $ qq form
          lift $ getListOrError val
        _ -> (:[]) <$> qq t
      (terms ++) <$> qqTerms ts
    
    qqImproper :: [Val] -> Val -> ReaderT Int EM Val
    qqImproper prop improp = do
      qqprop <- qqTerms prop
      qqimprop <- qq improp
      lift $ makeImproperMutableList qqprop qqimprop

quit :: Primitive
quit = Prim "quit" (Exactly 0) $ \_ -> throwError Quit

errorFunc :: Primitive
errorFunc = Prim "error" (Exactly 1) $ \case
  [val] 
    | stringSH val -> unwrapStringPH val >>= throwError . Default
    | otherwise -> liftIO (writeSharedSH val) >>= throwError . Default
