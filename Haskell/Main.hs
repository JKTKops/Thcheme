module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad (liftM)

import Parsers (readExpr)
import Evaluation (eval)
import LispVal (extractValue, trapError)
import Environment (Env, nullEnv, runIOThrows, liftThrows)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then runRepl
    else runOne $ head args
        
runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (prompt ">>> ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    input <- prompt
    if pred input
    then return ()
    else action input >> until_ pred prompt action

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ evaluationOutput where
    evaluationOutput = liftM show $ evaluation
    evaluation = (liftThrows $ readExpr expr) >>= eval env

prompt :: String -> IO String
prompt s = flushStr s >> getLine

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout
