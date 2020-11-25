module Evaluation
    ( -- | evaluate a string
      evaluate
      -- | evaluate a given expression
    , evaluateExpr
      -- | evaluate inside EM monad
    , eval
      -- | callCC (exported for use with EM)
    , callCC
    , runTest
      -- | Convert the evaluation output into a meaningful string
    , showResult
      -- | Function application, not sure why this is here rn
    , apply
    ) where

import Data.Maybe
import Data.Either
import Data.IORef
import Data.Array
import Control.Monad
import Control.Monad.Cont (runCont, callCC)
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

evalExpr :: LispVal -> EM LispVal
evalExpr expr = case expr of
    val@(String _) -> return val
    val@(Char _)   -> return val
    val@(Number _) -> return val
    val@(Bool _)   -> return val
    val@(Vector _) -> return val
    (Atom id)      -> getVar id
    nil@(List [])  -> return nil
    List (function : args) -> handleNonPrim function args
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

handleNonPrim :: LispVal -> [LispVal] -> EM LispVal
handleNonPrim function args = do
    func <- case function of
        Primitive {} -> return function
        Func {}      -> return function
        PMacro {}    -> return function
        _            -> eval function
    case func of
        Primitive {} -> evalCall func
        Func {}      -> evalCall func
        PMacro {}    -> evalPMacro func
        DottedList [Atom "macro"] macro@Func{} -> evalMacro macro
        _            -> evalCall func

  where evalCall func = do
            argVals <- mapM eval args
            let reduced = func `isReduced` function || args /= argVals
            when reduced $ do
                modifyTopReason $ const Reduce
                pushExpr Call (List (function : argVals))
            v <- apply func argVals
            when reduced popExpr
            return v
        evalPMacro pmacro = do
            modifyTopReason $ const Expand
            apply pmacro args
        evalMacro macro = do
            modifyTopReason $ const Expand
            expansion <- apply macro args
            eval expansion

        isReduced :: LispVal -> LispVal -> Bool
        isReduced _ (Atom _) = False
        isReduced new old    = new /= old

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
                      -- if we just use the captured scope, local defines will
                      -- see the outer-defined IORefs and overwrite them, so
                      -- we need to layer a fresh scope on top of the captured
                      -- one.
                      result <- withNewScope evalBody
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

apply notFunc _ = throwError $ NotFunction "Not a function" notFunc

num :: [a] -> Integer
num = toInteger . length

showResult :: (Either LispErr LispVal, EvalState) -> String
showResult res = case res of
    (Left e@(Parser _), _) -> show e ++ "\n"
    (Left err, s) -> show err ++ "\n" ++ show s
    (Right v, _)  -> show v

evalEM :: Env -> Opts -> EM LispVal -> IO (Either LispErr LispVal, EvalState)
evalEM initEnv opts (EM m) = runCont m (\v s -> pure (Right v, s)) $
                                ES [] [initEnv] opts

evaluate :: String -> Env -> Opts -> String -> IO (Either LispErr LispVal, EvalState)
evaluate label initEnv opts input = evalEM initEnv opts $ do
        v <- liftEither $ labeledReadExpr label input
        eval v

evaluateExpr :: Env -> Opts -> LispVal -> IO (Either LispErr LispVal, EvalState)
evaluateExpr env opts v = evalEM env opts $ eval v

-- | Provided for backwards compatibility.
runTest :: Env -> Opts -> EM LispVal -> IO (Either LispErr LispVal, EvalState)
runTest = evalEM
