{-
Scope sets.

Scopes are like graph colors. Each identifier is colored with the scopes
that it belongs to. Name resolution works by finding the largest scope set
which is a subset of the one attached to the name's syntax object.

We use the same trick as graph colors - each Scope is really just an Int.
-}
module Expander.Scopes where

import Data.IntSet

import Expander.Monad

newtype Scope = Scope Int

newScope :: Expand Scope
newScope = do
  scopeRef <- asks scopeCounter
  lift $ Scope <$> readRef scopeRef <* modifyRef' scopeRef (+1)

newtype Scopes = Scopes IntSet

-- | Add a scope to a scope set
addScope :: Scope -> Scopes -> Scopes
addScope (Scope i) (Scopes set) = Scopes $ insert i set

-- | Add a scope to a scope set if it is not present, and remove
-- it if it is present.
flipScope :: Scope -> Scopes -> Scopes
flipScope (Scope i) (Scopes set)
  -- in newer versions of 'containers',
  -- the 'alterF' function would do this nicely.
  -- It's not faster or anything, so low-priority.
  | i `member` set = Scopes $ delete i set
  | otherwise = Scopes $ insert i set

-- | @s1 `scopeSubset` s2@ is true iff s1 is a subset of s2.
scopeSubset :: Scopes -> Scopes -> Bool
scopeSubset (Scopes s1) (Scopes s2) = s1 `isSubsetOf` s2

noScopes :: Scopes
noScopes = Scopes empty
