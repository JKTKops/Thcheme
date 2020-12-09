{-# Language LambdaCase #-}
module Primitives.Math (primitives) where

import Control.Monad (mapM)

import Val
import EvaluationMonad (throwError)
import Primitives.Unwrappers (unwrapNum)
import Primitives.Bool (predicate)

addP, mulP, divP, modP, quotP, remP :: Primitive
addP = numericBinop "+" (+) 0
mulP = numericBinop "*" (*) 1
divP = guardDivZero $ numericBinop1 "/" div -- see Note: [divide]
modP = numericBinop1 "mod" mod
quotP = numericBinop1 "quotient" quot
remP = numericBinop1 "remainder" rem

-- | Converts a (Haskell) builtin function on Integers to a 'Primitive'.
--   The primitive can only cause type errors;
--   Too many arguments will be folded over the operation;
--   none will use the default value.
numericBinop :: String
             -> (Integer -> Integer -> Integer)
             -> Integer
             -> Primitive
numericBinop name op start = Prim name 0 $ fmap (Number . foldl op start) . mapM unwrapNum

-- | Same as numericBinop, except at least one arg must be given.
numericBinop1 :: String
              -> (Integer -> Integer -> Integer)
              -> Primitive
numericBinop1 name op = Prim name 1 $ fmap (Number . foldl1 op) . mapM unwrapNum

guardDivZero :: Primitive -> Primitive
guardDivZero (Prim name arity f) = Prim name arity $ \args ->
    if Number 0 `elem` tail args
    then throwError $ Default "divide by zero"
    else f args

-- | See the r7rs standard.
--
-- (- n) evaluates to -n, but (- x y z) evaluates to ((x - y) - z).
subP :: Primitive
subP = Prim "-" 1 $ \case
    [x]      -> Number . negate <$> unwrapNum x
    as@(_:_) -> Number . foldl1 (-) <$> mapM unwrapNum as
    []       -> throwError $ NumArgs 1 []

negateP :: Primitive
negateP = Prim "negate" 1 $ \case
    [x]     -> Number . negate <$> unwrapNum x
    badArgs -> throwError $ NumArgs 1 badArgs

numericPredicate :: String -> (Integer -> Bool) -> Primitive
numericPredicate name = predicate name unwrapNum

zeroCheck :: Primitive
zeroCheck = numericPredicate "zero?" (== 0)

positiveCheck :: Primitive
positiveCheck = numericPredicate "positive?" (> 0)

negativeCheck :: Primitive
negativeCheck = numericPredicate "negative?" (< 0)

oddCheck :: Primitive
oddCheck = numericPredicate "odd?" $ (0 /=) . mod 2

evenCheck :: Primitive
evenCheck = numericPredicate "even?" $ (0 ==) . mod 2

{- Note: [divide]
This has been commented out for a while now (12/6/2020) because we don't
support non-integer numbers. The above definition of divP technically meets
the standard, because we "fail nosily in an implementation-defined way" but
it's unsatisfactory because we could just support more numeric types.

divide :: RawPrimitive
divide []     = throwError $ NumArgs 1 []
divide [x]    = unwrapNum x >>= return . Number . ((Prelude./) 1)
divide params = mapM unwrapNum params >>= return . Number . foldl1 (Prelude./)
-}

primitives :: [Primitive]
primitives = [ addP
             , subP
             , mulP
             , divP
             , modP
             , quotP
             , remP
             , negateP
             , zeroCheck
             , positiveCheck
             , negativeCheck
             , oddCheck
             , evenCheck
             ]
