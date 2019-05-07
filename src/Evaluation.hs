module Evaluation
    ( evaluate -- ^ evaluate a string
    , evaluateExpr -- ^ evaluate a given expression
    , eval -- ^ evaluate inside EM monad
    , runTest
    , showResult -- ^ Convert the evaluation output into a meaningful string
    , apply -- ^ Function application, not sure why this is here rn
    ) where

import Data.Maybe
import Data.Either
import Data.IORef
import Data.Array
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Char as C (ord, chr)
import qualified Data.HashMap.Strict as Map

import Parsers
import Types
import EvaluationMonad
import qualified Environment as Env

eval :: LispVal -> EM LispVal
eval v = do
    pushExpr Call v
    res <- evalExpr v
    popExpr
    return res

-- TODO AFTER adding macros -> refactor the below into primitive macros
evalExpr :: LispVal -> EM LispVal
evalExpr expr = case expr of
    val@(String _) -> return val
    val@(Char _)   -> return val
    val@(Number _) -> return val
    val@(Bool _)   -> return val
    val@(Vector _) -> return val
    (Atom id)      -> getVar id
    nil@(List [])  -> return nil
    List (arg0@(Atom fName) : argExprs) ->
        let fPrim = case fName of
              "quote"       -> primQuote
              "if"          -> primIf
              "set!"        -> primSet
              "set-option!" -> primSetOpt
              "define"      -> primDefine
              "lambda"      -> primLambda
              "begin"       -> primBegin
              "eval"        -> primEval
              "apply"       -> primApply
              "load"        -> primLoad
              _             -> handleNonPrim arg0
        in fPrim argExprs
    List (function : args) -> handleNonPrim function args
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

-- TODO lift all primitives into EM and then as appropriate put these under primitives
primQuote :: [LispVal] -> EM LispVal
primQuote [v]     = return v
primQuote badArgs = throwError $ NumArgs 1 badArgs


primIf :: [LispVal] -> EM LispVal
primIf (pred : conseq : alts) = do
    p <- eval pred
    if truthy p
    then eval conseq
    else case alts of
        [] -> return $ List []
        xs -> last <$> mapM eval xs
primIf badArgs = throwError $ Default $ "Expected at least 2 args; found " ++ show badArgs


primSet :: [LispVal] -> EM LispVal
primSet [Atom var, form] = eval form >>= setVar var
primSet [notAtom, _] = throwError $ TypeMismatch "symbol" notAtom
primSet badArgs = throwError $ NumArgs 2 badArgs

primSetOpt :: [LispVal] -> EM LispVal
primSetOpt [Atom optName, form] = do
    val <- eval form
    state <- get
    let opts = options state
        opts' = Map.insert optName val opts
    put $ state { options = opts' }
    return val
primSetOpt [notAtom, _] = throwError $ TypeMismatch "symbol" notAtom
primSetOpt badArgs = throwError $ NumArgs 2 badArgs


primDefine :: [LispVal] -> EM LispVal
primDefine [Atom var, form] = eval form >>= \val ->
    let renamed = case val of
            Func {} -> val { name = Just var }
            _ -> val
    in defineVar var renamed
primDefine (List (Atom name : params) : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncNormal params body (Just name) >>= defineVar name

primDefine (DottedList (Atom name : params) varargs : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name

primDefine (List (notAtom : _) : _) = throwError $ TypeMismatch "symbol" notAtom
primDefine (DottedList (notAtom : _) _ : _) = throwError $ TypeMismatch "symbol" notAtom
primDefine (notAtomOrList : _) = throwError $ TypeMismatch "symbol or list" notAtomOrList
primDefine badArgs = throwError $ NumArgs 2 badArgs


primLambda :: [LispVal] -> EM LispVal
primLambda (List params : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncNormal params body Nothing
primLambda (DottedList params varargs : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs params body Nothing
primLambda (varargs@(Atom _) : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs [] body Nothing
primLambda (notAtomOrList : _) = throwError $ TypeMismatch "symbol or list" notAtomOrList
primLambda badArgs = throwError $ NumArgs 2 badArgs

emptyBodyError :: LispErr
emptyBodyError = Default "Attempt to define function with no body"


primBegin :: [LispVal] -> EM LispVal
primBegin []    = throwError $ Default "Expected at least 1 arg; found []"
primBegin stmts = last <$> mapM eval stmts


primEval :: [LispVal] -> EM LispVal
primEval [form]  = eval form
primEval badArgs = throwError $ NumArgs 1 badArgs

primApply :: [LispVal] -> EM LispVal
primApply [func, List args] = apply func args
primApply (func : args) = apply func args
primApply [] = throwError $ Default "Expected at least 1 arg; found []"

primLoad :: [LispVal] -> EM LispVal
primLoad [String filename] = do
    file <- liftIO . runExceptT $ load filename
    case file of
        Left e   -> throwError e
        Right ls -> last <$> mapM eval ls
primLoad [notString] = throwError $ TypeMismatch "string" notString
primLoad badArgs     = throwError $ NumArgs 1 badArgs


handleNonPrim :: LispVal -> [LispVal] -> EM LispVal
handleNonPrim function args = do
    func <- case function of
        Primitive {}   -> return function
        Func {}        -> return function
        PMacro {}      -> return function
        _              -> eval function
    case func of
        Primitive {} -> evalCall func
        Func {}      -> evalCall func
        PMacro {}    -> evalMacro func

  where evalCall func = do
            argVals <- mapM eval args
            let reduced = function /= func || args /= argVals
            when reduced $ do
                modifyTopReason $ const Reduce
                pushExpr Call (List (function : argVals))
            v <- apply func argVals
            when reduced popExpr
            return v
        evalMacro func = do
            modifyTopReason $ const Expand
            expansion <- apply func args
            eval expansion

apply :: LispVal -> [LispVal] -> EM LispVal

-- Applications of primitive functions
apply (Primitive arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

apply (PMacro arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

-- Applications of user-defined functions
apply (Func params varargs body closure name) args =
    case num args `compare` num params of
        -- Throw error if too many args and no varargs
        GT -> if isNothing varargs
              then throwError $ NumArgs (num params) args
        -- otherwise bind parameters in this closure, bind the varargs, and evaluate
              else evaluate
        EQ -> evaluate
        LT -> throwError $ NumArgs (num params) args

  where evaluate = do env <- makeBindings params
                      env' <- bindVarargs varargs env
                      pushEnv env'
                      result <- evalBody
                      popEnv
                      return result

        makeBindings :: [String] -> EM Env
        makeBindings vars = liftIO . Env.bindVars closure . Map.fromList $ zip vars args

        remainingArgs = drop (length params) args
        -- evaluate every expression in body, return value of the last one
        evalBody = last <$> mapM eval body
        -- binds extra arguments to vararg in GT case
        bindVarargs arg env = case arg of
          Just argName ->
              liftIO . Env.bindVars env $ Map.fromList [(argName, List remainingArgs)]
          Nothing -> return env

apply notFunc _ = throwError . NotFunction "Not a function" $ show notFunc

num :: [a] -> Integer
num = toInteger . length

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

truthy :: LispVal -> Bool
truthy v = not $ v == Bool False
              || v == Number 0
              || v == List []
              || v == String ""


showResult :: (Either LispErr LispVal, EvalState) -> String
showResult res = case res of
    (Left e@(Parser _), _) -> show e ++ "\n"
    (Left err, s) -> show err ++ "\n" ++ show s
    (Right v, _)  -> show v

evaluate :: Env -> Opts -> String -> IO (Either LispErr LispVal, EvalState)
evaluate initEnv opts input =
    flip runStateT (ES [] [initEnv] opts) . runExceptT . runEM $ do
        v <- liftEither $ readExpr input
        eval v

evaluateExpr :: Env -> Opts -> LispVal -> IO (Either LispErr LispVal, EvalState)
evaluateExpr env opts v =
    flip runStateT (ES [] [env] opts) . runExceptT . runEM $ eval v

runTest :: Env -> Opts -> EM LispVal -> IO (Either LispErr LispVal, EvalState)
runTest env opts m = flip runStateT (ES [] [env] opts) . runExceptT $ runEM m
