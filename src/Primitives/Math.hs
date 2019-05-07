{-# Language LambdaCase #-}
module Primitives.Math (rawPrimitives) where

import Control.Monad (mapM)
import Control.Monad.Except (throwError)

import Types
import Primitives.Unwrappers (unwrapNum)
import Primitives.Bool (predicate)

addP = numericBinop (+) 0
mulP = numericBinop (*) 1
divP = guardDivZero $ numericBinop1 div -- divide
modP = numericBinop1 mod
quotP = numericBinop1 quot
remP = numericBinop1 rem

-- | Converts a builtin function on Integers to a RawPrimitive.
--   The primitive can only cause type errors;
--   Too many arguments will be folded over the operation,
--   none will use the default value.
numericBinop :: (Integer -> Integer -> Integer)
             -> Integer
             -> RawPrimitive
numericBinop op start = RPrim 0 $ fmap (Number . foldl op start) . mapM unwrapNum

-- | Same as numericBinop, except at least one arg must be given.
numericBinop1 :: (Integer -> Integer -> Integer)
              -> RawPrimitive
numericBinop1 op = RPrim 1 $ fmap (Number . foldl1 op) . mapM unwrapNum

guardDivZero :: RawPrimitive -> RawPrimitive
guardDivZero (RPrim arity f) = RPrim arity $ \args ->
    if Number 0 `elem` tail args
    then throwError $ Default "divide by zero"
    else f args

subP = RPrim 1 $ \case
    [x]      -> Number . negate <$> unwrapNum x
    as@(_:_) -> (Number . foldl1 (-)) <$> mapM unwrapNum as
    []       -> throwError $ NumArgs 1 []

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

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("+", addP)
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
