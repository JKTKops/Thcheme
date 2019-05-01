module Main (main) where

import System.Environment
import Control.Monad.Except
import qualified Data.HashMap.Strict as Map (empty)

import Parsers (readExpr)
import Evaluation (evaluateExpr)
import LispVal (ThrowsError, IOThrowsError, LispVal(..), isTerminationError
               , extractValue, trapError, runIOThrows, liftThrows)
import Environment (Env, bindVar)
import Bootstrap (primitiveBindings)
import Repl

main :: IO ()
main = do
    args <- getArgs
    if null args
    then runRepl
    else runOne args

runOne :: [String] -> IO ()
runOne (filename : args) = do
    primEnv <- primitiveBindings
    env' <- bindVar primEnv "args" $ List . map String $ args
    result <- evaluateExpr env' Map.empty (List [Atom "load", String filename])
    case result of
        (Left err, s) -> print err >> print s
        _             -> return ()
