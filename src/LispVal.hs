module LispVal
  ( -- * LispVal and support types
    LispVal (..)
  , LispErr (..)
  , Primitive (..)
  , Macro (..)
  , Builtin

    -- * Manipulating pure 'LispVal's
  , truthy
  , canonicalizeList

    -- * Constructing primitive functions
  , makePrimitive
  ) where

import Types

makePrimitive :: Primitive -> LispVal
makePrimitive (Prim name arity func) = Primitive arity func name

-- | Canonicalize the form of an _immutable_ LispVal list.
-- if the input is not a list, or is not an immutable list,
-- it is returned unchanged.
--
-- I don't actually think this function is useful, an impure
-- function that canonicalizes any list would be much more useful.
canonicalizeList :: LispVal -> LispVal
canonicalizeList (DottedList lst (List cdr)) = List (lst ++ cdr)
canonicalizeList other = other