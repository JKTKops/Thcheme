module Main (main) where

import System.Environment

import Val
import Evaluation (evaluateExpr, initEvalState, showResultIO)
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
    env' <- bindVar primEnv "args" $ makeImmutableList 
                                   $ map (IString . pack) args
    initState <- initEvalState env' noOpts
    result <- evaluateExpr initState $
        makeImmutableList [Symbol "load", IString $ pack filename]
    case result of
        (Left{},_) -> 
            showResultIO result >>= putStrLn
        _             -> return ()
