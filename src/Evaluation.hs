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
import Control.Monad
import Control.Monad.Cont (runCont, callCC)
import qualified Data.HashMap.Strict as Map

import Parsers
import Val
import EvaluationMonad
import qualified Environment as Env

eval :: Val -> EM Val
eval v = do
    pushExpr Call v
    res <- evalExpr v
    popExpr
    return res

evalExpr :: Val -> EM Val
evalExpr expr = case expr of
    val@String{} -> return val
    val@Char{}   -> return val
    val@Number{} -> return val
    val@Bool{}   -> return val
    val@Vector{} -> return val
    (Atom id)    -> getVar id
    Nil          -> return Nil
    IList (function : args) -> handleNonPrim function args
    p@PairPtr{}  -> freezeList p >>= evalExpr
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

handleNonPrim :: Val -> [Val] -> EM Val
handleNonPrim function args = do
    func <- case function of
        Primitive{}    -> return function
        Continuation{} -> return function
        Func{}         -> return function
        PrimMacro{}    -> return function
        _              -> eval function
    case func of
        Primitive{}    -> evalCall func
        Continuation{} -> evalCall func
        Func{}         -> evalCall func
        PrimMacro{}    -> evalPMacro func
        -- TODO: this doesn't play well with the introduction of mutable
        -- lists, so it really is time to make a proper distinction between
        -- functions and low-level macros.
        IDottedList [Atom "macro"] macro@Func{} -> evalMacro macro
        _              -> evalCall func

  where evalCall func = do
            argVals <- mapM eval args
            let reduced = func `isReduced` function || args /= argVals
            when reduced $ do
                modifyTopReason $ const Reduce
                pushExpr Call (IList (function : argVals))
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

        isReduced :: Val -> Val -> Bool
        isReduced _ (Atom _) = False
        isReduced new old    = new /= old

apply :: Val -> [Val] -> EM Val

-- Applications of primitive functions
apply (Primitive arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

apply (PrimMacro arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

-- Application of continuation
apply (Continuation state func) [arg] = put state >> func arg
apply Continuation{} badArgs = throwError $ NumArgs 1 badArgs

-- Applications of user-defined functions
apply (Func params varargs body closure _name) args =
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
          Just argName -> do
              remainingArgsMutable <- makeMutableList remainingArgs
              liftIO . Env.bindVars env $ Map.fromList [(argName, remainingArgsMutable)]
          Nothing -> return env

apply notFunc _ = throwError $ NotFunction "Not a function" notFunc

num :: [a] -> Integer
num = toInteger . length

showResult :: (Either LispErr Val, EvalState) -> String
showResult res = case res of
    (Left e@(Parser _), _) -> show e ++ "\n"
    (Left err, s) -> show err ++ "\n" ++ show s
    (Right v, _)  -> show v

evalEM :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
evalEM initEnv opts (EM m) = runCont m (\v s -> pure (Right v, s)) $
                                ES [] [initEnv] opts

evaluate :: String -> Env -> Opts -> String -> IO (Either LispErr Val, EvalState)
evaluate label initEnv opts input = evalEM initEnv opts $
  labeledReadExpr label input >>= eval

evaluateExpr :: Env -> Opts -> Val -> IO (Either LispErr Val, EvalState)
evaluateExpr env opts v = evalEM env opts $ eval v

-- | Provided for backwards compatibility.
runTest :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
runTest = evalEM
