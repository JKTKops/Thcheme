{-# LANGUAGE LambdaCase #-}
module Primitives.Misc (primitives, macros) where

-- TODO split into Control and Syntax

import qualified Data.HashMap.Lazy as Map

import Parsers (load)
import LispVal
import Evaluation
import EvaluationMonad
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
    callcc :: [LispVal] -> EM LispVal
    callcc [func] = callCC $ \k -> do
        cont <- reifyCont k
        -- k :: LispVal -> EM b
        -- need to produce EM LispVal
        apply func [cont]
    callcc badArgs = throwError $ NumArgs 1 badArgs

    reifyCont :: (LispVal -> EM LispVal) -> EM LispVal
    reifyCont k = do s <- get
                     return $ Continuation s k

-- compose?

quote :: Macro
quote = Macro 1 $ \case
    [form]  -> return form
    badArgs -> throwError $ NumArgs 1 badArgs

ifMacro :: Macro
ifMacro = Macro 2 $ \case
    (pred : conseq : alts) -> do
        p <- eval pred
        if truthy p
        then eval conseq
        else case alts of
            [] -> return $ IList []
            xs -> last <$> mapM eval xs
    badArgs -> throwError $ Default $ "Expected at least 2 args; found " ++ show badArgs

set :: Macro
set = Macro 2 $ \case
    [Atom var, form] -> eval form >>= setVar var
    [notAtom, _]     -> throwError $ TypeMismatch "symbol" notAtom
    badArgs          -> throwError $ NumArgs 2 badArgs

setOpt :: Macro
setOpt = Macro 2 $ \case
    [Atom optName, form] -> do
        val <- eval form
        modify $ \s -> s { options = Map.insert optName val (options s) }
        return val
    [notAtom, _] -> throwError $ TypeMismatch "symbol" notAtom
    badArgs      -> throwError $ NumArgs 2 badArgs

defineBuiltin :: Builtin
-- arity is AtLeast 1 so can't be called with no args
defineBuiltin (x:xs) = do
  defnHead <- freezeList x
  defineBuiltinFrozen defnHead xs

defineBuiltinFrozen :: LispVal -> [LispVal] -> EM LispVal
defineBuiltinFrozen (Atom var) [form] = do -- todo: NumArgs (Exact 2) when app.
  setVarForCapture var
  val <- eval form
  let renamed = case val of
        Func {} -> val { name = Just var }
        _ -> val
  defineVar var renamed

defineBuiltinFrozen (IList (Atom name : params)) body = case body of
  [] -> throwError emptyBodyError
  _  -> makeFuncNormal params body (Just name) >>= defineVar name

defineBuiltinFrozen (IDottedList (Atom name : params) varargs) body = 
  case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name

defineBuiltinFrozen (IList (notAtom : _)) _ = 
  throwError $ TypeMismatch "symbol" notAtom

defineBuiltinFrozen (IDottedList (notAtom : _) _) _ =
  throwError $ TypeMismatch "symbol" notAtom

defineBuiltinFrozen notAtomOrList _ = 
  throwError $ TypeMismatch "symbol or list" notAtomOrList

-- arities are AtLeast 1 so that we can throw a nicer error than just
-- "expects at least 2 arguments" in the case of defining a function.
define, lambda :: Macro
define = Macro 1 defineBuiltin
lambda = Macro 1 mkLambda

mkLambda :: [LispVal] -> EM LispVal
-- arity is AtLeast 1 so can't be called with no args
mkLambda (formals : body) = do 
  frozenFormals <- freezeList formals
  case frozenFormals of
    IList params -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body Nothing
    IDottedList params varargs -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs params body Nothing
    varargs@(Atom _) -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs [] body Nothing
    notAtomOrList -> throwError $ TypeMismatch "symbol or list" notAtomOrList

emptyBodyError :: LispErr
emptyBodyError = Default "Attempt to define function with no body"

makeFunc :: Maybe String
         -> [LispVal]
         -> [LispVal]
         -> Maybe String
         -> EM LispVal
makeFunc varargs params body name = do
    maybe (pure ()) setVarForCapture name
    env <- envSnapshot
    return $ Func (map show params) varargs body env name

makeFuncNormal :: [LispVal] -> [LispVal] -> Maybe String -> EM LispVal
makeFuncNormal = makeFunc Nothing
makeFuncVarargs :: LispVal -> [LispVal] -> [LispVal] -> Maybe String -> EM LispVal
makeFuncVarargs = makeFunc . Just . show

defmacro :: Macro
defmacro = Macro 3 $ \case
    (Atom name : ps : body) -> do
        lam <- mkLambda (ps : body)
        let macro = IDottedList [Atom "macro"] lam
        defineVar name macro

begin :: Macro
begin = Macro 1 $ \case
    []    -> throwError $ NumArgs 1 []
    stmts -> last <$> mapM eval stmts

evalPrim :: Primitive
evalPrim = Prim "eval" 1 $ \case
    [form]  -> eval form
    badArgs -> throwError $ NumArgs 1 badArgs

-- TODO: [r7rs] (also it's just borked rn because of Pair)
applyFunc :: Primitive
applyFunc = Prim "apply" 1 $ \case
    [func, form] -> do
      frozenForm <- freezeList form
      case frozenForm of
        IList args -> apply func args
        bad -> throwError $ TypeMismatch "list" bad
    (func : args) -> apply func args
    [] -> throwError $ Default "Expected at least 1 arg; found []"

loadPrim :: Primitive
loadPrim = Prim "load" 1 $ \case
    [String filename] -> do
        file <- liftIO . runExceptT $ load filename
        case file of
            Left e   -> throwError e
            Right ls -> last <$> mapM eval ls
    [notString] -> throwError $ TypeMismatch "string" notString
    badArgs     -> throwError $ NumArgs 1 badArgs

-- TODO: [r7rs]
-- the lists that quasiquote _outputs_ need to be mutable!
quasiquote :: Macro
quasiquote = Macro 1 $ \case
    [form]  -> runReaderT (qq form) 0
    badArgs -> throwError $ NumArgs 1 badArgs

  where qq :: LispVal -> ReaderT Int EM LispVal
        qq term = lift (freezeList term) >>= \case
          IList [Atom "quasiquote", form] -> local (+ 1) $ do
            inner <- qq form
            return $ IList [Atom "quasiquote", inner]
          IList [Atom "unquote", form] -> do
            depth <- ask
            if depth == 0
            then lift $ eval form
            else local (subtract 1) $ do
              inner <- qq form
              return $ IList [Atom "unquote", inner]
          IList forms
              -- this can happen because the parser is smart enough to rewrite
              -- `(0 . ,lst) as IList [Number 0, Atom unquote, Atom lst]
              -- however whenever this happens, ',lst' will definitely be of size 2
            | Atom "unquote" `elem` forms
            , (list, dot) <- break (== Atom "unquote") forms
            , [_,_] <- dot
            -> fmap canonicalizeList $ IDottedList <$> qqTerms list <*> qq (IList dot)
            | otherwise
            -> IList <$> qqTerms forms
          IDottedList forms form ->
            fmap canonicalizeList $ IDottedList <$> qqTerms forms <*> qq form
          form -> return form

        qqTerms :: [LispVal] -> ReaderT Int EM [LispVal]
        qqTerms [] = return []
        qqTerms (t:ts) = do
          lift $ pushExpr Expand t
          frozenT <- lift $ freezeList t
          terms <- case frozenT of
            IList [Atom "unquote-splicing", form] -> do
              depth <- ask
              val <- if depth == 0
                     then lift $ eval form
                     else local (subtract 1) $ qq form
              lift $ getListOrError val
            _ -> (:[]) <$> qq t
          lift popExpr
          (terms ++) <$> qqTerms ts

quit :: Primitive
quit = Prim "quit" 0 $ \_ -> throwError Quit

errorFunc :: Primitive
errorFunc = Prim "error" 1 $ \case
    [String s] -> throwError $ Default s
    [notStr]   -> throwError . Default $ show notStr
    badArgs    -> throwError $ NumArgs 1 badArgs
