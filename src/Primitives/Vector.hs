{-# LANGUAGE LambdaCase #-}
module Primitives.Vector
  ( primitives
  
  -- * Haskell-level vector utilities
  , vectorLengthPH, vectorElemsPH
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Functor (($>))
import Control.Monad.Except

import Val
import EvaluationMonad (EM, panic)

import GHC.Stack

primitives :: [Primitive]
primitives = [vectorP, makeVectorP, vectorLengthP, vectorRefP, vectorSetP]

vectorP :: Primitive
vectorP = Prim "vector" 1 $ liftIO . fmap Vector . V.thaw . V.fromList

makeVectorB :: Builtin
makeVectorB [val] = makeVectorB [val, Number 0]
makeVectorB [Number k, val]
  | k < 0 = throwError $ TypeMismatch "positive number" (Number k)
  | k > toInteger (maxBound :: Int) = throwError $ Default $
    "can't make vector bigger than " ++ show (maxBound :: Int)
  | otherwise = liftIO $ Vector <$> MV.replicate (fromInteger k) val
makeVectorB [notNum, _] = throwError $ TypeMismatch "number" notNum
makeVectorB badArgs = throwError $ NumArgs 1 badArgs

makeVectorP :: Primitive
makeVectorP = Prim "make-vector" 1 makeVectorB

vectorLengthP :: Primitive
vectorLengthP = Prim "vector-length" 1 $ \case
    [val] | vectorSH val -> return $ Number $ toInteger $ vectorLengthPH val
          | otherwise -> throwError $ TypeMismatch "vector" val
    badArgs -> throwError $ NumArgs 1 badArgs

vectorRefP :: Primitive
vectorRefP = Prim "vector-ref" 2 $ \case
    [vec, num]
      | vectorSH vec 
      , Number k <- num 
      -> vectorRefPHS vec (fromInteger k)
      | not $ vectorSH vec 
      -> throwError $ TypeMismatch "vector" vec
      | otherwise
      -> throwError $ TypeMismatch "number" num
    badArgs       -> throwError $ NumArgs 2 badArgs

vectorRefPHS :: Val -> Int -> EM Val
vectorRefPHS v i = do
    if i < 0 || i >= vectorLengthPH v 
      then throwError $ Default $ "vector index out of bounds: " ++ show i
      else case v of
        Vector mv  -> liftIO $ MV.read mv i
        IVector iv -> return $ iv V.! i

vectorSetP :: Primitive
vectorSetP = Prim "vector-set!" 3 vectorSetB

vectorSetB :: Builtin
vectorSetB [Vector v, Number k, obj] = vectorSetHHSS v (fromInteger k) obj
vectorSetB [IVector{}, _, _] = throwError $ SetImmutable "vector"
vectorSetB [notVec, Number{}, _] = throwError $ TypeMismatch "vector" notVec
vectorSetB [_, notNum, _] = throwError $ TypeMismatch "number" notNum
vectorSetB badArgs = throwError $ NumArgs 3 badArgs

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
