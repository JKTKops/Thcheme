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

zeroCheck :: RawPrimitive
zeroCheck [x]     = Bool . (== 0) <$> unwrapNum x
zeroCheck badArgs = throwError $ NumArgs 1 badArgs

positiveCheck :: RawPrimitive
positiveCheck [x]     = Bool . (> 0) <$> unwrapNum x
positiveCheck badArgs = throwError $ NumArgs 1 badArgs

negativeCheck :: RawPrimitive
negativeCheck [x]     = Bool . (< 0) <$> unwrapNum x
negativeCheck badArgs = throwError $ NumArgs 1 badArgs

oddCheck :: RawPrimitive
oddCheck [x]     = Bool . (0 ==) . mod 2 <$> unwrapNum x
oddCheck badArgs = throwError $ NumArgs 1 badArgs

evenCheck :: RawPrimitive
evenCheck [x]     = Bool . (0 /=) . mod 2 <$> unwrapNum x
evenCheck badArgs = throwError $ NumArgs 1 badArgs

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
             , ("zero?", zeroCheck)
             , ("positive?", positiveCheck)
             , ("negative?", negativeCheck)
             , ("odd?", oddCheck)
             , ("even?", evenCheck)
             ]
