{-# LANGUAGE LambdaCase #-}
module Primitives.Bool (rawPrimitives, predicate, boolBinop) where

import Control.Monad.Except (throwError)

import Types
import Primitives.Unwrappers (unwrapBool)

rawPrimitives = [ ("&&", boolAnd)
                , ("||", boolOr)
                , ("not", boolNot)
                ]

predicate :: (LispVal -> ThrowsError a)
          -> (a -> Bool)
          -> RawPrimitive
predicate unwrapper p = RPrim 1 $ \case
    [val] -> Bool . p <$> unwrapper val
    badArgs -> throwError $ NumArgs 1 badArgs

boolNot :: RawPrimitive
boolNot = predicate unwrapBool not

boolBinop :: (LispVal -> ThrowsError a) -- unwrapper
          -> (a -> a -> Bool) -- binary boolean operation
          -> RawPrimitive
boolBinop unwrapper op = RPrim 2 $ \case
    [left, right] -> do
        left' <- unwrapper left
        right' <- unwrapper right
        return . Bool $ left' `op` right'
    badArgs       -> throwError $ NumArgs 2 badArgs

boolBoolBinop = boolBinop unwrapBool

boolAnd = boolBoolBinop (&&)
boolOr  = boolBoolBinop (||)
