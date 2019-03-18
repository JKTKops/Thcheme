module Main where

import System.Environment
import System.IO (hFlush, stdout)

import LispVal
import Parsers (readExpr)
import Evaluation (eval)

main :: IO ()
main = do
  expr <- prompt ">>> "
  print . eval $ readExpr expr
  main
  where
    prompt s = do
      putStr s
      hFlush stdout
      getLine
