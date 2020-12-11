{-# LANGUAGE ExistentialQuantification #-}
module Primitives.Comparison (primitives) where

import Control.Monad.Except (throwError)

import Types -- when the equivalence functions are updated to work properly,
             -- only import Val!
import Primitives.Bool (boolBinop)
import Primitives.Unwrappers

primitives :: [Primitive]
primitives = typeSpecific
          ++ [ Prim name 2 $ liftEither . eqf
             | (name, eqf) <- [("eq?", eqv), ("eqv?", eqv)]
             ]

typeSpecific :: [Primitive]
typeSpecific = [ primGen builtin
               | (PrimBuilder primGen) <- primBuilders, builtin <- builtinComparisons
               ]

builtinComparisons :: Ord a => [(String, a -> a -> Bool)]
builtinComparisons = [ ("=", (==))
                     , ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     ]

-- | A wrapper over a function that converts a (Haskell) builtin comparison
-- function into a Primitive function. We use this to convert the Haskell
-- builtins into appropriate comparison functions for all Scheme types.
data PrimBuilder = forall a. Ord a =>
                      PrimBuilder ((String, a -> a -> Bool) -> Primitive)

primBuilders :: [PrimBuilder]
primBuilders = [ PrimBuilder makeNumPrim
               , PrimBuilder makeStrPrim
               , PrimBuilder makeCharPrim
               ]
  where makePrim :: String -- type name
                 -> Bool   -- appends '?' if False, nothing if True
                 -> (String -> (a -> a -> Bool) -> Primitive)
                 -> (String, a -> a -> Bool)
                 -> Primitive
        makePrim tyname isNum primGen (opName, op) = primGen primName op
          where primName = tyname ++ opName ++ suffix
                suffix | isNum = "?"
                       | otherwise = ""

        makeNumPrim = makePrim "" True numBoolBinop
        makeStrPrim = makePrim "string" False strBoolBinop
        makeCharPrim = makePrim "char" False charBoolBinop

-- boolBinop :: String -> (Val -> EM a) -> (a -> a -> Bool) -> Primitive

-- TODO: none of these satisfy r7rs, which says the predicate should be
-- satisfied pairwise along a whole list of elements; we should not use
-- 'boolBinop'.
numBoolBinop :: String -> (Integer -> Integer -> Bool) -> Primitive
numBoolBinop name = boolBinop name unwrapNum
strBoolBinop :: String -> (String -> String -> Bool) -> Primitive
strBoolBinop name = boolBinop name unwrapStr
charBoolBinop :: String -> (Char -> Char -> Bool) -> Primitive
charBoolBinop name = boolBinop name unwrapChar

-- EQUIVALENCE FUNCTIONS
-- TODO some notion of function equivalence?

-- TODO [r7rs]
-- This function absolutely does not satisfy r7rs.
eqv :: [Val] -> ThrowsError Val
eqv [Bool x, Bool y]     = return . Bool $ x == y
eqv [Number x, Number y] = return . Bool $ x == y
eqv [Char x, Char y]     = return . Bool $ x == y
eqv [String s, String t] = return . Bool $ s == t
eqv [Atom x, Atom y]     = return . Bool $ x == y
eqv [_, _]  = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

-- TODO: instead of defining coercers, we should just import TypeTransformers
-- and implement the standard properly.
-- furthermore, we aren't defining case-insensitive string/char comparisons.
-- Once we have char-foldcase and string-foldcase we should just call those
-- similar to how we'd call a type transformer.
