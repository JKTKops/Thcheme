module Main (main) where

import System.Environment

import Val
import Evaluation (evaluateExpr, showResultIO)
import Environment (bindVar)
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
    env' <- bindVar primEnv "args" $ makeImmutableList $ map String $ args
    result <- evaluateExpr env' noOpts $
        makeImmutableList [Atom "load", String filename]
    case result of
        (Left{},_) -> 
            showResultIO result >>= putStrLn
        _             -> return ()
