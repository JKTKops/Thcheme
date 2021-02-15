{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Primitives.Math (primitives) where

import Control.Monad (mapM)
import Data.Complex (Complex(..))
import Data.Functor ((<&>))
import Data.List (foldl', foldl1')

import Val
import EvaluationMonad (throwError, panic)
import Primitives.Unwrappers (unwrapNum, unwrapRealNum)
import Primitives.Bool (predicate, predicateM)

addP, mulP, divP, modP, quotP, remP :: Primitive
addP = numericBinop "+" (+) 0
mulP = numericBinop "*" (*) 1
divP = guardDivZero $ numericBinop1 "/" (/) -- see Note: [divide]
modP = realBinop1 "mod" mod
quotP = realBinop1 "quotient" quot
remP = realBinop1 "remainder" rem

-- | Converts a (Haskell) builtin function on Integers to a 'Primitive'.
--   The primitive can only cause type errors;
--   Too many arguments will be folded over the operation;
--   none will use the default value.
numericBinop :: String
             -> (Number -> Number -> Number)
             -> Number
             -> Primitive
numericBinop name op start = Prim name (AtLeast 0) $
  fmap (Number . foldl' op start) . mapM unwrapNum

-- | Same as numericBinop, except at least one arg must be given.
numericBinop1 :: String
              -> (Number -> Number -> Number)
              -> Primitive
numericBinop1 name op = Prim name (AtLeast 1) $
  fmap (Number . foldl1' op) . mapM unwrapNum

realBinop1 :: String
           -> (RealNumber -> RealNumber -> RealNumber)
           -> Primitive
realBinop1 name op = Prim name (AtLeast 1) $
  fmap (Number . Real . foldl1' op) . mapM unwrapRealNum

guardDivZero :: Primitive -> Primitive
guardDivZero (Prim name arity f) = Prim name arity $ \args ->
    if any isExactZeroVal $ tail args
    then throwError $ Default "divide by zero"
    else f args
  where
    isExactZeroVal (Number n) = isExactZero n
    isExactZeroVal _ = False

-- | See the r7rs standard.
--
-- (- n) evaluates to -n, but (- x y z) evaluates to ((x - y) - z).
subP :: Primitive
subP = Prim "-" (AtLeast 1) $ \case
  [x]      -> Number . negate <$> unwrapNum x
  as@(_:_) -> Number . foldl1 (-) <$> mapM unwrapNum as
  _ -> panic "sub arity"

negateP :: Primitive
negateP = Prim "negate" (Exactly 1) $
  \[x] -> Number . negate <$> unwrapNum x

numericPredicate :: String -> (RealNumber -> Bool) -> Primitive
numericPredicate name p = predicateM name $ fmap p . unwrapRealNum

zeroCheck :: Primitive
zeroCheck = predicate "zero?" unwrapNum (== 0)

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

inexactP :: Primitive
inexactP = Prim "inexact" (Exactly 1) $
  \[x] -> unwrapNum x <&> \case
    Real r -> Number $ Real $ realInexact r
    Complex (r :+ i) -> Number $ Complex $ realInexact r :+ realInexact i

makeRectangularP :: Primitive
makeRectangularP = Prim "make-rectangular" (Exactly 2) $
  \[x, y] -> do
    a <- unwrapRealNum x
    b <- unwrapRealNum y
    return $ Number $ Complex $ a :+ b

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

             , inexactP

             , makeRectangularP
             ]
