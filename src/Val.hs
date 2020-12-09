{-# LANGUAGE PatternSynonyms #-}
module Val
  ( -- * Val and support types
    Val (.., Nil), PairObj (..)
  , LispErr (..), isTerminationError
  , Primitive (..)
  , Macro (..)
  , Builtin

    -- * Manipulating pure 'Val's
  , truthy
  , canonicalizeList

    -- * Constructing primitive functions
  , makePrimitive

    -- * Basic handling of lists
  , getList, getListOrError, freezeList, testCircularList
  , isList, requireList, makeMutableList, cons

    -- * Direct handling of pairs
  , carRef, cdrRef, derefCar, derefCdr, setCarRef, setCdrRef

    -- * Test for immutable data
  , isImmutablePair
  ) where

import Types
import EvaluationMonad
import Control.Monad (when)
import Control.Monad.Trans.Maybe (MaybeT(..))

makePrimitive :: Primitive -> Val
makePrimitive (Prim name arity func) = Primitive arity func name

-- | Pattern for @IList []@, the Nil object.
pattern Nil :: Val
pattern Nil = IList []

-- | Canonicalize the form of an _immutable_ Val list.
-- if the input is not a list, or is not an immutable list,
-- it is returned unchanged.
--
-- Canonicalizing a mutable list doesn't really make sense,
-- since both mutable proper and improper lists are just chains
-- of pairs.
canonicalizeList :: Val -> Val
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
getList :: Val -> EM (Maybe [Val])
getList v = do
  b <- isList v
  if b
    then Just . fst <$> flattenFiniteList v
    else return Nothing

-- | 'getList', but if the object is not a list, a type error is raised
-- instead of returning 'Nothing'.
getListOrError :: Val -> EM [Val]
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
freezeList :: Val -> EM Val
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
requireFiniteList :: Val -> EM ()
requireFiniteList v = whenM (testCircularList v) (throwError CircularList)

-- | Test if a Scheme value is a circular list.
testCircularList :: Val -> EM Bool
testCircularList v =  next v >>= \case
    Nothing   -> pure False
    Just node -> lift2 go (pure $ Just node) (next node)
  where
    -- Floyd's Algorithm
    go tortoise hare
      | Just t <- tortoise
      , Just h <- hare
      = if t == h
        then return True
        else lift2 go (next t) (next2 h)
      | otherwise = return False

    next (PairPtr pair) = Just <$> derefCdr pair
    next _ = pure Nothing

    next2 = runMaybeT . (next' >=> next')
      where next' = MaybeT . next

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
flattenFiniteList :: Val -> EM ([Val], Val)
flattenFiniteList (IList xs) = return (xs, Nil)
flattenFiniteList (IDottedList xs dot) = return (xs, dot)
flattenFiniteList (PairPtr pair) = do
  hd <- derefCar pair
  ~(tl, dot) <- derefCdr pair >>= flattenFiniteList
  return (hd:tl, dot)
flattenFiniteList other = return ([], other)

-- | Make a freshly-allocated mutable Scheme list containing
-- the given values.
makeMutableList :: [Val] -> EM Val
makeMutableList [] = pure Nil
makeMutableList (v:vs) = do
  tl <- makeMutableList vs
  cons v tl

-- | Primitive list? operation.
--
-- See page 42 of the r7rs report:
-- "By definition, all lists have finite length 
-- and are terminated by the empty list."
isList :: Val -> EM Bool
isList v = callCC $ \exit -> do
  whenM (testCircularList v) $ exit False
  (_, tl) <- flattenFiniteList v
  case tl of
    Nil -> return True
    _   -> return False

-- | Primitive cons operation.
cons :: Val -> Val -> EM Val
cons car cdr = do
  pair <- PairObj <$> newRef car <*> newRef cdr
  PairPtr <$> newRef pair

-- | Get the Ref to the car of a pair.
carRef :: Ref PairObj -> EM (Ref Val)
carRef pair = do
  PairObj c _d <- readRef pair
  return c

-- | Get the Ref to the cdr of a pair.
cdrRef :: Ref PairObj -> EM (Ref Val)
cdrRef pair = do
  PairObj _c d <- readRef pair
  return d

derefCar :: Ref PairObj -> EM Val
derefCar ref = carRef ref >>= readRef

derefCdr :: Ref PairObj -> EM Val
derefCdr ref = cdrRef ref >>= readRef

setCarRef :: Ref PairObj -> Val -> EM ()
setCarRef pair v = carRef pair >>= flip writeRef v

setCdrRef :: Ref PairObj -> Val -> EM ()
setCdrRef pair v = cdrRef pair >>= flip writeRef v

-- | Require that the given Val is a list. Otherwise, raises a 'TypeError'.
requireList :: Val -> EM ()
requireList v =
  whenM (not <$> isList v) $ throwError $ TypeMismatch "list" v

whenM :: Monad m => m Bool -> m () -> m ()
whenM test thing = do
  b <- test
  when b thing

-- | Test if a given Scheme object is an immutable pair.
isImmutablePair :: Val -> Bool
isImmutablePair IList{} = True
isImmutablePair IDottedList{} = True
isImmutablePair _ = False
