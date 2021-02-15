{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Primitives.Vector
  ( primitives
  
  -- * Haskell-level vector utilities
  , vectorLengthPH, vectorElemsPH
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Functor (($>))
import Control.Monad.Except ( MonadIO(..), MonadError(throwError) )

import Val
import EvaluationMonad (EM, panic)
import Primitives.Unwrappers (unwrapExactInteger)

primitives :: [Primitive]
primitives = [vectorP, makeVectorP, vectorLengthP, vectorRefP, vectorSetP]

vectorP :: Primitive
vectorP = Prim "vector" (AtLeast 0) $ 
  liftIO . fmap Vector . V.thaw . V.fromList

makeVectorB :: Builtin
makeVectorB [val] = makeVectorB [val, Number 0]
makeVectorB [n@(getExactInteger -> Just k), val]
  | k < 0 = throwError $ TypeMismatch "exact positive integer" n
  | k > toInteger (maxBound :: Int) = throwError $ Default $
    "can't make vector bigger than " ++ show (maxBound :: Int)
  | otherwise = liftIO $ Vector <$> MV.replicate (fromInteger k) val
makeVectorB [notNum, _] = throwError $ TypeMismatch "number" notNum
makeVectorB _ = panic "makeVector arity"

makeVectorP :: Primitive
makeVectorP = Prim "make-vector" (Between 1 2) makeVectorB

vectorLengthP :: Primitive
vectorLengthP = Prim "vector-length" (Exactly 1) $ \case
  [val] | vectorSH val -> return $ makeBignum $ toInteger $ vectorLengthPH val
        | otherwise -> throwError $ TypeMismatch "vector" val
  _ -> panic "vectorLength arity"

vectorRefP :: Primitive
vectorRefP = Prim "vector-ref" (Exactly 2) $ \case
  [vec, num]
    | vectorSH vec -> do
      k <- unwrapExactInteger num
      vectorRefPHS vec (fromInteger k)
    | otherwise -> throwError $ TypeMismatch "vector" vec
  _ -> panic "vectorRef arity"

vectorRefPHS :: Val -> Int -> EM Val
vectorRefPHS v i = do
  if i < 0 || i >= vectorLengthPH v 
    then throwError $ Default $ "vector index out of bounds: " ++ show i
    else case v of
      Vector mv  -> liftIO $ MV.read mv i
      IVector iv -> return $ iv V.! i
      _ -> panic "vectorRef type"

vectorSetP :: Primitive
vectorSetP = Prim "vector-set!" (Exactly 3) vectorSetB

vectorSetB :: Builtin
vectorSetB [Vector v, getExactInteger -> Just k, obj] = 
  vectorSetHHSS v (fromInteger k) obj
vectorSetB [IVector{}, _, _] = throwError $ SetImmutable "vector"
vectorSetB [notVec, Number{}, _] = throwError $ TypeMismatch "vector" notVec
vectorSetB [_, notNum, _] = throwError $ TypeMismatch "number" notNum
vectorSetB _ = panic "vectorSet arity"

vectorSetHHSS :: MV.IOVector Val -> Int -> Val -> EM Val
vectorSetHHSS v i obj = do
    if i < 0 || i >= MV.length v
      then throwError $ Default $ "vector index out of bounds: " ++ show i
      else liftIO $ MV.write v i obj $> obj

-------------------------------------
-- internal utilities for vectors
-------------------------------------

vectorLengthPH :: Val -> Int
vectorLengthPH (Vector v)  = MV.length v
vectorLengthPH (IVector v) = V.length v
vectorLengthPH _ = panic "vectorLengthPH: not a vector"

vectorElemsPH :: (MonadIO m) => Val -> m [Val]
vectorElemsPH (Vector v) = liftIO $ V.toList <$> V.freeze v
vectorElemsPH (IVector v) = return $ V.toList v
vectorElemsPH _ = panic "vectorElemsPH: not a vector"
