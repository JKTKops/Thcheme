{-# LANGUAGE LambdaCase #-}
module Primitives.Vector (rawPrimitives, macros) where

import Data.Array
import Control.Monad.Except

import Types
import Evaluation (eval)
import EvaluationMonad (updateWith)

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("vector", vector)
                , ("make-vector", makeVector)
                , ("vector-length", vectorLength)
                , ("vector-ref", vectorRef)
                ]

macros :: [(String, Macro)]
macros = [ ("vector-set!", vectorSet)
         ]

vector :: RawPrimitive
vector = RPrim 1 $ \vals -> return . Vector $
    listArray (0, fromIntegral $ length vals - 1) vals

makeVector :: RawPrimitive
makeVector = RPrim 1 $ \case
    [Number n] -> return . Vector $ listArray (0, n - 1) (repeat $ Number 0)
    [Number n, val] -> return . Vector $ listArray (0, n - 1) (repeat val)
    (notNum : _) -> throwError $ TypeMismatch "number" notNum
    badArgs -> throwError $ NumArgs 1 badArgs

vectorLength :: RawPrimitive
vectorLength = RPrim 1 $ \case
    -- lower bound omitted, always 0
    [Vector arr] -> let (_, hi) = bounds arr in return . Number $ hi + 1
    [notVec]     -> throwError $ TypeMismatch "vector" notVec
    badArgs      -> throwError $ NumArgs 1 badArgs

vectorRef :: RawPrimitive
vectorRef = RPrim 2 $ \case
    [Vector arr, Number i]
        | i `elem` [0.. snd (bounds arr)] -> return $ arr ! i
        | otherwise -> throwError . Default $ "Vector index out of bounds: " ++ show i
    [Vector _, notNum] -> throwError $ TypeMismatch "number" notNum
    [notVec, _] -> throwError $ TypeMismatch "vector" notVec
    badArgs -> throwError $ NumArgs 2 badArgs

vectorSet :: Macro
vectorSet = Macro 3 $ \case
    args@(Atom _ : _) -> updateWith helper args
    args -> helper args
  where helper :: [LispVal] -> EM LispVal
        helper args = do
            let head : tail = args
            argVals <- mapM eval tail
            case head : argVals of
                [Vector arr, Number i, val]
                    | i `elem` [0.. snd (bounds arr)] ->
                      return . Vector $ arr // [(i, val)]
                    | otherwise ->
                      throwError . Default $ "Vector index out of bounds: " ++ show i
                [Vector _, notNum, _] -> throwError $ TypeMismatch "number" notNum
                [notVec, _, _] -> throwError $ TypeMismatch "vector" notVec
                badArgs -> throwError $ NumArgs 3 badArgs
