module Primitives.Bool (primitives, predicate, predicateM, boolBinop) where

import Val
import EvaluationMonad (EM, panic)
import Primitives.Unwrappers (unwrapBool)

primitives :: [Primitive]
primitives = [ boolNot
             , boolAnd
             , boolOr
             ]

-- | Evaluate a predicate on a 'Val'. It turns out to be convenient
-- (at least, currently, 12/6/2020) in Math to separate an impure projection
-- from the 'Val' and a pure predicate on the projected value.
predicate :: Symbol        -- ^ name of resulting 'Primitive'
          -> (Val -> EM a) -- ^ impure projection
          -> (a -> Bool)   -- ^ pure predicate
          -> Primitive
predicate name unwrapper p = Prim name (Exactly 1) $
  \ [val] -> Bool . p <$> unwrapper val

-- | Evaluate an impure predicate on a 'Val'.
predicateM :: Symbol           -- ^ name of resulting Primitive
           -> (Val -> EM Bool) -- ^ predicate (SH)
           -> Primitive
predicateM name p = Prim name (Exactly 1) $
  \ [val] -> Bool <$> p val

boolNot :: Primitive
boolNot = Prim "not" (Exactly 1) $ \case
  [Bool False] -> pure (Bool True)
  [_]          -> pure (Bool False)
  _ -> panic "primitive 'not' arity"

-- | Evaluate a binary boolean function on two 'Val's. See 'predicate'.
boolBinop :: Symbol            -- ^ name of resulting 'Primitive'
          -> (Val -> EM a)     -- ^ impure projection
          -> (a -> a -> Bool)  -- ^ pure binary boolean operation
          -> Primitive
boolBinop name unwrapper op = Prim name (AtLeast 2) $
  \args -> do
    unwrappedArgs <- mapM unwrapper args
    let pairedByOp = zipWith op unwrappedArgs $ tail unwrappedArgs
    return $ Bool $! and pairedByOp

-- TODO: [r7rs]
-- neither of these functions are defined (surprinsgly?) in the r7rs report.
-- Instead, r7rs relies on the macros "and" and "or".
-- Probably the right thing to do is remove them.
boolAnd, boolOr :: Primitive
boolAnd = boolBinop "&&" unwrapBool (&&)
boolOr  = boolBinop "||" unwrapBool (||)
