{-# LANGUAGE LambdaCase #-}
module Primitives.Misc (rawPrimitives, macros) where

import Data.IORef
import qualified Data.HashMap.Lazy as Map

import Parsers (load)
import Types
import Evaluation
import EvaluationMonad

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("id", identityFunction) ]

macros :: [(String, Macro)]
macros = [ ("quote", quote)
         , ("quasiquote", quasiquote)
         , ("if", ifMacro)
         , ("set!", set)
         , ("set-option!", setOpt)
         , ("define", define)
         , ("lambda", lambda)
         , ("begin", begin)
         , ("eval", evalMacro)
         , ("apply", applyMacro)
         , ("load", loadMacro)
         ]

identityFunction :: RawPrimitive
identityFunction = RPrim 1 $ \case
    [arg]   -> return arg
    badArgs -> throwError $ NumArgs 1 badArgs

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
            [] -> return $ List []
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

define :: Macro
define = Macro 1 $ \case
    [Atom var, form] -> do
        val <- eval form
        let renamed = case val of
                Func {} -> val { name = Just var }
                _ -> val
        defineVar var renamed
    (List (Atom name : params) : body) -> case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncNormal params body (Just name) >>= defineVar name
    (DottedList (Atom name : params) varargs : body) -> case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name
    (List (notAtom : _) :_) -> throwError $ TypeMismatch "symbol" notAtom
    (DottedList (notAtom : _) _ : _) -> throwError $ TypeMismatch "symbol" notAtom
    (notAtomOrList : _) -> throwError $ TypeMismatch "symbol or list" notAtomOrList
    badArgs -> throwError $ NumArgs 2 badArgs

lambda :: Macro
lambda = Macro 1 $ \case
    (List params : body) -> case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncNormal params body Nothing
    (DottedList params varargs : body) -> case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncVarargs varargs params body Nothing
    (varargs@(Atom _) : body) -> case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncVarargs varargs [] body Nothing
    (notAtomOrList : _) -> throwError $ TypeMismatch "symbol or list" notAtomOrList
    badArgs -> throwError $ NumArgs 2 badArgs

emptyBodyError :: LispErr
emptyBodyError = Default "Attempt to define function with no body"

makeFunc :: Maybe String
         -> [LispVal]
         -> [LispVal]
         -> Maybe String
         -> EM LispVal
makeFunc varargs params body name = do
    emptyEnv <- liftIO $ newIORef Map.empty
    return $ Func (map show params) varargs body emptyEnv name

makeFuncNormal = makeFunc Nothing
makeFuncVarargs = makeFunc . Just . show

begin :: Macro
begin = Macro 1 $ \case
    []    -> throwError $ Default "Expected at least 1 arg; found []"
    stmts -> last <$> mapM eval stmts

evalMacro :: Macro
evalMacro = Macro 1 $ \case
    [form]  -> eval form
    badArgs -> throwError $ NumArgs 1 badArgs

applyMacro :: Macro
applyMacro = Macro 1 $ \case
    [func, List args] -> apply func args
    (func : args) -> apply func args
    [] -> throwError $ Default "Expected at least 1 arg; found []"

loadMacro :: Macro
loadMacro = Macro 1 $ \case
    [String filename] -> do
        file <- liftIO . runExceptT $ load filename
        case file of
            Left e   -> throwError e
            Right ls -> last <$> mapM eval ls
    [notString] -> throwError $ TypeMismatch "string" notString
    badArgs     -> throwError $ NumArgs 1 badArgs

quasiquote :: Macro
quasiquote = Macro 1 qq
  where qq :: [LispVal] -> EM LispVal
        qq [List [Atom "unquote", form]] = eval form
        qq [List forms] = List <$> qqTerms forms
        qq [form]  = return form
        qq badArgs = throwError $ NumArgs 1 badArgs

        qqTerms :: [LispVal] -> EM [LispVal]
        qqTerms [] = return []
        qqTerms (t:ts) = do
            pushExpr Expand t
            terms <- case t of
                List [Atom "unquote", form] -> (:[]) <$> eval form
                List [Atom "unquote-splicing", form] -> do
                    val <- eval form
                    case val of
                        List vs -> return vs
                        _ -> throwError $ TypeMismatch "list" val
                _ -> (:[]) <$> qq [t]
            popExpr
            (terms ++) <$> qqTerms ts
