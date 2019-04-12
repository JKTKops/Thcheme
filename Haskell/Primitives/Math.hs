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
             -> RawPrimitive
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = Number . foldl1 op <$> mapM unwrapNum params

subtract :: RawPrimitive
subtract []     = throwError $ NumArgs 1 []
subtract [x]    = Number . negate <$> unwrapNum x
subtract params = numericBinop (-) params

{-divide :: RawPrimitive
divide []     = throwError $ NumArgs 1 []
divide [x]    = unwrapNum x >>= return . Number . ((Prelude./) 1)
divide params = mapM unwrapNum params >>= return . Number . foldl1 (Prelude./)-}

primitives :: [(String, RawPrimitive)]
primitives = [ ("+", addP)
             , ("-", subP)
             , ("*", mulP)
             , ("/", divP)
             , ("mod", modP)
             , ("quotient", quotP)
             , ("remainder", remP)
             ]
