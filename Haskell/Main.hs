module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad (liftM)

import Parsers (readExpr)
import Evaluation (eval)
import LispVal (extractValue, trapError)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
    then runRepl
    else evalAndPrint $ head args
        
runRepl :: IO ()
runRepl = until_ (== "quit") (prompt ">>> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
    then return ()
    else action result >> until_ pred prompt action

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return . extractValue $ evaluationOutput where
    evaluationOutput = trapError . liftM show $ evaluation
    evaluation = readExpr expr >>= eval

prompt :: String -> IO String
prompt s = flushStr s >> getLine

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout
