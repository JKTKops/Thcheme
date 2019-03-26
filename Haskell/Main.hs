module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad.Except (liftM, runExceptT)

import Parsers (readExpr)
import Evaluation (eval)
import LispVal (ThrowsError, LispVal, isTerminationError, extractValue, trapError)
import Environment (Env, primitiveBindings, runIOThrows, liftThrows)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then runRepl
    else runOne $ head args
        
runRepl :: IO ()
runRepl = primitiveBindings >>= \env -> 
    until_ isTerminationError (prompt ">>> " >>= evalString env) printResult

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

until_ :: Monad m
       => (a -> Bool) -- ^ predicate executed on the result of action
       -> m a         -- ^ monadic action which evaluates to the argument for action
       -> (a -> m ()) -- ^ function to call if pred succeeds
       -> m ()        -- ^ Compound action which sequences (prompt >>= action) until pred fails.
until_ pred prompt action = do
    result <- prompt
    if pred result
    then return ()
    else action result >> until_ pred prompt action

printResult :: ThrowsError LispVal -> IO ()
printResult result = putStrLn $ asString
    where asString = extractValue . trapError $ liftM show result 

evalString :: Env -> String -> IO (ThrowsError LispVal)
evalString env expr = runExceptT $ evaluation
  where evaluation = parse >>= eval env
        parse      = liftThrows $ readExpr expr

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= printResult 

prompt :: String -> IO String
prompt s = flushStr s >> getLine

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout
