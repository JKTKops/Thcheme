{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Primitives.Math (primitives) where

import Control.Monad (mapM)
import Data.Complex (Complex(..))
import Data.Functor ((<&>))
import Data.List (foldl', foldl1')
import Data.Ratio (approxRational)

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

-------------------------------------------------------------------------------
-- typically useful math operations
-------------------------------------------------------------------------------

inexactP :: Primitive
inexactP = Prim "inexact" (Exactly 1) $
  \[x] -> unwrapNum x <&> \case
    Real r -> Number $ Real $ realInexact r
    Complex (r :+ i) -> Number $ Complex $ realInexact r :+ realInexact i

rationalizeP :: Primitive
rationalizeP = Prim "rationalize" (Exactly 2) $
  \[x, eps] -> do
    rx <- unwrapRealNum x
    reps <- unwrapRealNum eps
    return $ Number $ Real $ Ratnum $ approxRational rx reps

squareP :: Primitive
squareP = Prim "square" (Exactly 1) $
  \[x] -> Number . (\n -> n*n) <$> unwrapNum x

exptP :: Primitive
exptP = Prim "expt" (Exactly 2) $
  \[z1, z2] -> do
    z1' <- unwrapNum z1
    z2' <- unwrapNum z2
    return $ Number $ z1' ** z2'

-------------------------------------------------------------------------------
-- Complex functions
-------------------------------------------------------------------------------

makeRectangularP :: Primitive
makeRectangularP = Prim "make-rectangular" (Exactly 2) $
  \[x, y] -> do
    a <- unwrapRealNum x
    b <- unwrapRealNum y
    return $ Number $ Complex $ a :+ b

makePolarP :: Primitive
makePolarP = Prim "make-polar" (Exactly 2) $
  \[m, theta] -> do
    m' <- unwrapRealNum m
    theta' <- unwrapRealNum theta
    return $ Number $ Real m' * Complex (cos theta' :+ sin theta')

realPartP :: Primitive
realPartP = Prim "real-part" (Exactly 1) $
  \[z] -> unwrapNum z <&> \case
    Real r -> Number $ Real r
    Complex (r :+ _i) -> Number $ Real r

imagPartP :: Primitive
imagPartP = Prim "imag-part" (Exactly 1) $
  \[z] -> unwrapNum z <&> \case
    Real _r -> Number $ Real 0
    Complex (_r :+ i) -> Number $ Real i

magnitudeP :: Primitive
magnitudeP = Prim "magnitude" (Exactly 1) $
  \[z] -> Number . abs <$> unwrapNum z

angleP :: Primitive
angleP = Prim "angle" (Exactly 1) $
  \[z] -> unwrapNum z <&> \case
    Real r
      | r < 0 || isNegativeZero r -> Number pi
      | otherwise -> Number 0
    Complex (a :+ b) -> Number $ Real $ atan2 b a

liftFloatingMonop :: (Number -> Number) -> Builtin
liftFloatingMonop f = asBuiltin
  where asBuiltin [x] = do
          nx <- unwrapNum x
          return $ Number $ f nx
        asBuiltin _ = panic "liftFloatingMonop arity"

makeFloatingMonopPrim :: (String, Number -> Number) -> Primitive
makeFloatingMonopPrim (name, f) = Prim name (Exactly 1) $ liftFloatingMonop f

floatingMonopPs :: [Primitive]
floatingMonopPs = map makeFloatingMonopPrim
  [ ("exp", exp)
  , ("sqrt", sqrt)
  , ("sin", sin)
  , ("cos", cos)
  , ("tan", tan)
  , ("asin", asin)
  , ("acos", acos)
  ]

logP :: Primitive
logP = Prim "log" (Between 1 2) $ \case
  [z] -> Number . log <$> unwrapNum z
  [z1, z2] -> do
    z1' <- unwrapNum z1
    z2' <- unwrapNum z2
    return $ Number $ logBase z2' z1'
  _ -> panic "logB arity"

atanP :: Primitive
atanP = Prim "atan" (Between 1 2) $ \case
  [z] -> Number . atan <$> unwrapNum z
  [y, x] -> do
    y' <- unwrapRealNum y
    x' <- unwrapRealNum x
    return $ Number $ Real $ atan2 y' x'
  _ -> panic "atanB arity"

-------------------------------------------------------------------------------

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
             , rationalizeP
             , atanP
             , squareP
             , exptP

             , makeRectangularP
             , makePolarP
             , realPartP, imagPartP
             , magnitudeP, angleP
             , logP
             ]
             ++ floatingMonopPs
