{-# LANGUAGE LambdaCase #-}
module Primitives.Bool (primitives, predicate, boolBinop) where

import Control.Monad.Except (throwError)

import LispVal
import EvaluationMonad (EM)
import Primitives.Unwrappers (unwrapBool)

primitives :: [Primitive]
primitives = [ boolNot
             , boolAnd
             , boolOr
             ]

-- | Evaluate a predicate on a 'LispVal'. It turns out to be convenient
-- (at least, currently, 12/6/2020) in Math to separate an impure projection
-- from the 'LispVal' and a pure predicate on the projected value.
predicate :: String            -- ^ name of resulting 'Primitive'
          -> (LispVal -> EM a) -- ^ impure projection
          -> (a -> Bool)       -- ^ pure predicate
          -> Primitive
predicate name unwrapper p = Prim name 1 $ \case
    [val] -> Bool . p <$> unwrapper val
    badArgs -> throwError $ NumArgs 1 badArgs

boolNot :: Primitive
boolNot = predicate "not" unwrapBool not

-- | Evaluate a binary boolean function on two 'LispVal's. See 'predicate'.
boolBinop :: String            -- ^ name of resulting 'Primitive'
          -> (LispVal -> EM a) -- ^ impure projection
          -> (a -> a -> Bool)  -- ^ pure binary boolean operation
          -> Primitive
boolBinop name unwrapper op = Prim name 2 $ \case
    [left, right] -> do
        left'  <- unwrapper left
        right' <- unwrapper right
        return $ Bool $ left' `op` right'
    badArgs       -> throwError $ NumArgs 2 badArgs

-- TODO: [r7rs]
-- neither of these functions are defined (surprinsgly?) in the r7rs report.
-- Instead, r7rs relies on the macros "and" and "or".
-- Probably the right thing to do is remove them.
boolAnd, boolOr :: Primitive
boolAnd = boolBinop "&&" unwrapBool (&&)
boolOr  = boolBinop "||" unwrapBool (||)
