module Primitives.Math (primitives) where

import Control.Monad (mapM)
import Control.Monad.Except (throwError)

import LispVal 
import Primitives.Unwrappers (unwrapNum)

addP = numericBinop (+)
subP = Primitives.Math.subtract
mulP = numericBinop (*)
divP = numericBinop div -- divide
modP = numericBinop mod
quotP = numericBinop quot
remP = numericBinop rem

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unwrapNum params >>= return . Number . foldl1 op

subtract :: [LispVal] -> ThrowsError LispVal
subtract []     = throwError $ NumArgs 1 []
subtract [x]    = unwrapNum x >>= return . Number . negate
subtract params = numericBinop (-) params

{-divide :: [LispVal] -> ThrowsError LispVal
divide []     = throwError $ NumArgs 1 []
divide [x]    = unwrapNum x >>= return . Number . ((Prelude./) 1)
divide params = mapM unwrapNum params >>= return . Number . foldl1 (Prelude./)-}

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", addP)
             , ("-", subP)
             , ("*", mulP)
             , ("/", divP)
             , ("mod", modP)
             , ("quotient", quotP)
             , ("remainder", remP)
             ]
