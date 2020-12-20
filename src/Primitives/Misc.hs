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
identityFunction = Prim "id" 1 $ \case
    [arg]   -> return arg
    badArgs -> throwError $ NumArgs 1 badArgs

callWithCurrentContinuation :: Primitive
callWithCurrentContinuation = Prim "call-with-current-continuation" 1 callcc
  where
    callcc :: [Val] -> EM Val
    callcc [func] = callCC $ \k ->
        tailCall func [Continuation k]
    callcc badArgs = throwError $ NumArgs 1 badArgs

-- compose?

quote :: Macro
quote = Macro 1 $ \_ -> \case
    [form]  -> return form
    badArgs -> throwError $ NumArgs 1 badArgs

ifMacro :: Macro
ifMacro = Macro 2 $ \tail -> \case
    (pred : conseq : alts) -> do
        p <- evalBody pred
        if truthy p
        then eval tail conseq
        else case alts of
            [] -> return Nil -- r7rs says unspecified
            xs -> evalSeq tail xs
    badArgs -> throwError $ Default $ "Expected at least 2 args; found " ++ show badArgs

set :: Macro
set = Macro 2 $ \_ -> \case
    [Atom var, form] -> evalBody form >>= setVar var
    [notAtom, _]     -> throwError $ TypeMismatch "symbol" notAtom
    badArgs          -> throwError $ NumArgs 2 badArgs

setOpt :: Macro
setOpt = Macro 2 $ \_ -> \case
    [Atom optName, form] -> do
        val <- evalBody form
        let mopt = readMaybe optName
        case mopt of
          Nothing -> pure ()
          Just opt -> if truthy val then enableOpt opt else disableOpt opt
        return val
    [notAtom, _] -> throwError $ TypeMismatch "symbol" notAtom
    badArgs      -> throwError $ NumArgs 2 badArgs

defineBuiltin :: Builtin
-- arity is AtLeast 1 so can't be called with no args
defineBuiltin (x:xs) = do
  defnHead <- freezeList x
  defineBuiltinFrozen defnHead xs

  where
    defineBuiltinFrozen :: FrozenList -> [Val] -> EM Val
    defineBuiltinFrozen (FNotList (Atom var)) [form] = do -- todo: NumArgs (Exact 2) when app.
      setVarForCapture var
      val <- evalBody form
      let renamed = case val of
            Closure{} -> val { name = Just var }
            _ -> val
      defineVar var renamed
    
    defineBuiltinFrozen (FList (Atom name : params)) body = case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body (Just name) >>= defineVar name
    
    defineBuiltinFrozen (FDottedList (Atom name : params) varargs) body = 
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
define, lambda :: Macro
define = Macro 1 $ const defineBuiltin
lambda = Macro 1 $ const mkLambda

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
    FNotList varargs@(Atom _) -> case body of
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
defmacro = Macro 3 $ const $ \case
    (Atom name : ps : body) -> do
        lam <- mkLambda (ps : body)
        macro <- makeImproperMutableList [Atom "macro"] lam
        defineVar name macro

begin :: Macro
begin = Macro 1 $ \tail -> \case
    []    -> throwError $ NumArgs 1 []
    stmts -> evalSeq tail stmts

evalPrim :: Primitive
evalPrim = Prim "eval" 1 $ \case
    [form]  -> evalTail form
    badArgs -> throwError $ NumArgs 1 badArgs

-- TODO: [r7rs]
applyFunc :: Primitive
applyFunc = Prim "apply" 1 $ \case
    [func, form] -> do
      frozenForm <- freezeList form
      case frozenForm of
        FList args -> tailCall func args
        _bad -> throwError $ TypeMismatch "list" form
    (func : args) -> tailCall func args
    [] -> throwError $ Default "Expected at least 1 arg; found []"

loadPrim :: Primitive
loadPrim = Prim "load" 1 $ \case
  [val] 
    | stringSH val -> do
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
  badArgs     -> throwError $ NumArgs 1 badArgs

quasiquote :: Macro
quasiquote = Macro 1 $ \_ -> \case
    [form]  -> runReaderT (qq form) 0
    badArgs -> throwError $ NumArgs 1 badArgs

  where qq :: Val -> ReaderT Int EM Val
        qq term = lift (freezeList term) >>= \case
          FList [Atom "quasiquote", form] -> local (+ 1) $ do
            inner <- qq form
            lift $ makeMutableList [Atom "quasiquote", inner]
          FList [Atom "unquote", form] -> do
            depth <- ask
            if depth == 0
            then lift $ evalBody form
            else local (subtract 1) $ do
              inner <- qq form
              lift $ makeMutableList [Atom "unquote", inner]
          FList forms
              -- this can happen because the parser is smart enough to rewrite
              -- `(0 . ,lst) as IList [Number 0, Atom unquote, Atom lst]
              -- however whenever this happens, ',lst' will definitely be of size 2
            | Atom "unquote" `elem` forms
            , (list, dot) <- break (== Atom "unquote") forms
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
            FList [Atom "unquote-splicing", form] -> do
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
quit = Prim "quit" 0 $ \_ -> throwError Quit

errorFunc :: Primitive
errorFunc = Prim "error" 1 $ \case
  [val] 
    | stringSH val -> unwrapStringPH val >>= throwError . Default
    | otherwise -> liftIO (writeSharedSH val) >>= throwError . Default
  badArgs -> throwError $ NumArgs 1 badArgs
