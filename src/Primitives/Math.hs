{-# Language LambdaCase #-}
module Primitives.Math (primitives) where

import Control.Monad (mapM)
import Control.Monad.Except (throwError)

import LispVal 
import Primitives.Unwrappers (unwrapNum)
import Primitives.Bool (predicate)

addP = numericBinop (+)
subP = numericBinop (-) 
mulP = numericBinop (*)
divP = numericBinop div -- divide
modP = numericBinop mod
quotP = numericBinop quot
remP = numericBinop rem

-- | Converts a builtin function on Integers to a RawPrimitive.
--   The primitive can only cause type errors;
--   Too many arguments will be folded over the operation,
--   too few will trigger partial application.
numericBinop :: (Integer -> Integer -> Integer)
             -> RawPrimitive
numericBinop op = RPrim 2 $ fmap (Number . foldl1 op) . mapM unwrapNum

negateP :: RawPrimitive
negateP = RPrim 1 $ \case
    [x]     -> Number . negate <$> unwrapNum x
    badArgs -> throwError $ NumArgs 1 badArgs

numericPredicate :: (Integer -> Bool) -> RawPrimitive
numericPredicate = predicate unwrapNum

zeroCheck :: RawPrimitive
zeroCheck = numericPredicate (== 0)

positiveCheck :: RawPrimitive
positiveCheck = numericPredicate (> 0)

negativeCheck :: RawPrimitive
negativeCheck = numericPredicate (< 0)

oddCheck :: RawPrimitive
oddCheck = numericPredicate $ (0 /=) . mod 2

evenCheck :: RawPrimitive
evenCheck = numericPredicate $ (0 ==) . mod 2

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
             , ("negate", negateP)
             , ("zero?", zeroCheck)
             , ("positive?", positiveCheck)
             , ("negative?", negativeCheck)
             , ("odd?", oddCheck)
             , ("even?", evenCheck)
             ]
