module Main (main) where

import System.Environment
import Control.Monad.Except
import qualified Data.HashMap.Strict as Map (empty)

import Types
import Parsers (readExpr)
import Evaluation (evaluateExpr)
import Environment (Env, bindVar)
import Bootstrap (primitiveBindings)
import Repl

main :: IO ()
main = do
    args <- getArgs
    if null args
    then repl
    else runOne args

runOne :: [String] -> IO ()
runOne (filename : args) = do
    primEnv <- primitiveBindings
    env' <- bindVar primEnv "args" $ IList . map String $ args
    result <- evaluateExpr env' Map.empty (IList [Atom "load", String filename])
    case result of
        (Left err, s) -> print err >> print s
        _             -> return ()
