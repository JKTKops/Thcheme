module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad.Except (liftM, runExceptT)

import Parsers (readExpr)
import Evaluation (eval)
import LispVal (ThrowsError, IOThrowsError, LispVal, isTerminationError
               , extractValue, trapError, runIOThrows, liftThrows)
import Environment (Env, primitiveBindings)

main :: IO ()
main = do
    args <- getArgs
    if null args
    then runRepl
    else runOne $ head args
        
runRepl :: IO ()
runRepl = primitiveBindings >>= \env -> 
    until_ isTerminationError 
           (prompt ">>> " >>= runExceptT . evalString env) 
           (printResult . liftThrows)

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

printResult :: IOThrowsError LispVal -> IO ()
printResult result = ioResult >>= putStrLn
    where ioResult = asString <$> runExceptT result 
          asString = extractValue . trapError . showSuccess
          showSuccess =  fmap show

evalString :: Env -> String -> IOThrowsError LispVal
evalString env expr = evaluation
  where evaluation = parse >>= eval env
        parse      = liftThrows $ readExpr expr

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env = printResult . evalString env

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine
