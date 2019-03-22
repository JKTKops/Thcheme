module Primitives.Math 
    ( (Primitives.Math.+)
    , (Primitives.Math.-)
    , (Primitives.Math.*)
    , (Primitives.Math./)
    , Primitives.Math.mod
    , Primitives.Math.rem
    , Primitives.Math.quot
    ) where

import Control.Monad (mapM)
import Control.Monad.Except (throwError)

import LispVal 
import Primitives.Unwrappers (unwrapNum)

(+) = numericBinop (Prelude.+)
(-) = Primitives.Math.subtract
(*) = numericBinop (Prelude.*)
(/) = numericBinop div  -- Primitives.Math.divide
mod = numericBinop Prelude.mod
quot = numericBinop Prelude.quot
rem = numericBinop Prelude.rem

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unwrapNum params >>= return . Number . foldl1 op

subtract :: [LispVal] -> ThrowsError LispVal
subtract []     = throwError $ NumArgs 1 []
subtract [x]    = unwrapNum x >>= return . Number . negate
subtract params = mapM unwrapNum params >>= return . Number . foldl1 (Prelude.-)

{-divide :: [LispVal] -> ThrowsError LispVal
divide []     = throwError $ NumArgs 1 []
divide [x]    = unwrapNum x >>= return . Number . ((Prelude./) 1)
divide params = mapM unwrapNum params >>= return . Number . foldl1 (Prelude./)-}
