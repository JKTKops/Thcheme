module Main (main) where

import System.Environment
import Control.Monad.Except

import Types
import Parsers (readExpr)
import Evaluation (evaluateExpr)
import Environment (Env, bindVar)
import Bootstrap (primitiveBindings)
import Options (noOpts)
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
    result <- evaluateExpr env' noOpts (IList [Atom "load", String filename])
    case result of
        (Left err, s) -> print err >> print s
        _             -> return ()
