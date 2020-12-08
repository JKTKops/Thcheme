{-# LANGUAGE PatternSynonyms #-}
module LispVal
  ( -- * LispVal and support types
    LispVal (.., Nil)
  , LispErr (..)
  , Primitive (..)
  , Macro (..)
  , Builtin

    -- * Manipulating pure 'LispVal's
  , truthy
  , canonicalizeList

    -- * Constructing primitive functions
  , makePrimitive

    -- * Basic handling of lists
  , getList, getListOrError, freezeList, testCircularList
  , isList, requireList, makeMutableList, cons

    -- * Test for immutable data
  , isImmutablePair
  ) where

import Types
import EvaluationMonad
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT(..))

makePrimitive :: Primitive -> LispVal
makePrimitive (Prim name arity func) = Primitive arity func name

-- | Pattern for @IList []@, the Nil object.
pattern Nil :: LispVal
pattern Nil = IList []

-- | Canonicalize the form of an _immutable_ LispVal list.
-- if the input is not a list, or is not an immutable list,
-- it is returned unchanged.
--
-- Canonicalizing a mutable list doesn't really make sense,
-- since both mutable proper and improper lists are just chains
-- of pairs.
canonicalizeList :: LispVal -> LispVal
-- since Nil = IList [], the first case turns
-- IDottedList xs Nil into IList xs as it should.
canonicalizeList (IDottedList [] obj) = obj
canonicalizeList (IDottedList lst (IList cdr)) = IList (lst ++ cdr)
canonicalizeList (IDottedList lst1 (IDottedList lst2 dot)) =
  canonicalizeList (IDottedList (lst1 ++ lst2) dot)
canonicalizeList other = other

{- Note: [List Operation Efficiency]
There's some low-hanging fruit here for optimizing list operations.
Primarily, most operations end up walking the structure more than once.
'testCircularList' walks the whole list (in the event that the list is
not circular) since it terminates when it sees a cdr that isn't a Pair.
But then 'flattenFiniteList' also walks the whole list, to flatten it,
and is always called separately from 'testCircularList'. Notably,
'isList' currently tests, then flattens to check the ending. But then
'getList' calls 'isList', then flattens to get the list! So 'getList'
ends up walking the whole structure 3 times.

I think the most frequently-called function here should end up being
'freezeList'
-}

-- | Flatten a Scheme list into a Haskell list.
--
-- If the Scheme value is not a list (in the sense of 'list?')
-- then 'Nothing' is returned. Otherwise, 'Just' the contents of the
-- list are returned.
getList :: LispVal -> EM (Maybe [LispVal])
getList v = do
  b <- isList v
  if b
    then Just . fst <$> flattenFiniteList v
    else return Nothing

-- | 'getList', but if the object is not a list, a type error is raised
-- instead of returning 'Nothing'.
getListOrError :: LispVal -> EM [LispVal]
getListOrError v = do
  mlst <- getList v
  case mlst of
    Nothing  -> throwError $ TypeMismatch "list" v
    Just lst -> return lst

-- | Get an immutable representation of a mutable Scheme list.
-- This is useful for core operations like `eval`, which can evaluate
-- a mutable list by freezing it and then evaluating the frozen list instead.
-- Attempting to freeze a circular mutable list raises a CircularList error.
--
-- If the object is not a mutable list, it is returned unchanged.
freezeList :: LispVal -> EM LispVal
freezeList v = do
  requireFiniteList v
  (lst, dot) <- flattenFiniteList v
  case (lst, dot) of
    ([], obj) -> return obj
    (xs, Nil) -> return $ IList xs
    (xs, obj) -> return $ IDottedList xs obj

-- | Require that a Scheme value is not a circular list. 
-- Circular data structures that are not lists may or may not be detected. 
-- If the value is a circular list, a CircularList error is raised.
requireFiniteList :: LispVal -> EM ()
requireFiniteList v = whenM (testCircularList v) (throwError CircularList)

-- | Test if a Scheme value is a circular list.
testCircularList :: LispVal -> EM Bool
testCircularList v = case nextV v of
    Nothing -> pure False
    Just ref -> lift2 go (pure $ Just ref) (nextR ref)
  where
    -- Floyd's Algorithm
    go tortoise hare
      | Just t <- tortoise
      , Just h <- hare
      = if t == h
        then return True
        else lift2 go (nextR t) (nextR2 h)
      | otherwise = return False

    nextV (Pair _car cdr) = Just cdr
    nextV _               = Nothing
    nextR = fmap nextV . readRef
    nextR2 = runMaybeT . (nextR' >=> nextR')
      where nextR' = MaybeT . nextR

    lift2 f x y = do
      xv <- x
      yv <- y
      f xv yv

-- | Flatten a _finite_ Scheme list into an init/improper tail pair.
-- A result of (init, tl) means that "(init . tl)" is an external
-- representation of the input object.
--
-- See 'freezeList', which wraps this function and further converts
-- the pair to a proper Scheme object.
flattenFiniteList :: LispVal -> EM ([LispVal], LispVal)
flattenFiniteList (IList xs) = return (xs, Nil)
flattenFiniteList (IDottedList xs dot) = return (xs, dot)
flattenFiniteList (Pair car cdr) = do
  hd <- readRef car
  ~(tl, dot) <- readRef cdr >>= flattenFiniteList
  return (hd:tl, dot)
flattenFiniteList other = return ([], other)

-- | Make a freshly-allocated mutable Scheme list containing
-- the given values.
makeMutableList :: [LispVal] -> EM LispVal
makeMutableList [] = pure Nil
makeMutableList (v:vs) = do
  tl <- makeMutableList vs
  cons v tl

-- | Primitive cons operation.
cons :: LispVal -> LispVal -> EM LispVal
cons car cdr = Pair <$> newRef car <*> newRef cdr

-- | Primitive list? operation.
--
-- See page 42 of the r7rs report:
-- "By definition, all lists have finite length 
-- and are terminated by the empty list."
isList :: LispVal -> EM Bool
isList v = callCC $ \exit -> do
  whenM (testCircularList v) $ exit False
  (_, tl) <- flattenFiniteList v
  case tl of
    Nil -> return True
    _   -> return False

-- | Require that the given LispVal is a list. Otherwise, raises a 'TypeError'.
requireList :: LispVal -> EM ()
requireList v =
  whenM (not <$> isList v) $ throwError $ TypeMismatch "list" v

whenM :: Monad m => m Bool -> m () -> m ()
whenM test thing = do
  b <- test
  when b thing

-- | Test if a given Scheme object is immutable.
isImmutablePair :: LispVal -> Bool
isImmutablePair IList{} = True
isImmutablePair IDottedList{} = True
isImmutablePair _ = False
