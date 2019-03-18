module Main where

import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad (liftM)

import Parsers (readExpr)
import Evaluation (eval)
import LispVal (extractValue, trapError)

main :: IO ()
main = do
  expr <- prompt ">>> "
  evaled <- return . liftM show $ readExpr expr >>= eval
  putStrLn . extractValue $ trapError evaled
  main
  where
    prompt s = do
      putStr s
      hFlush stdout
      getLine
