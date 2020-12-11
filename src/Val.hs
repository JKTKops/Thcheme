module Val
  ( -- * Val and support types
    Val (..), PairObj (..)
  , LispErr (..), isTerminationError
  , Primitive (..)
  , Macro (..)
  , Builtin
  -- Don't export ConstRef, because we want consumers to go through
  -- the functions that do linting.

    -- * Manipulating pure 'Val's
  , truthy

    -- * Constructing primitive functions
  , makePrimitive

    -- * Basic handling and construction of lists
  , getList, getListOrError, testCircularList
  , isListSH, requireList, makeMutableList, makeImmutableList
  , consSSS, makeImproperMutableList, makeImproperImmutableList

    -- * Freezing lists for evaluation
  , FrozenList (..), freezeList, lintFrozenList

    -- * Direct handling of pairs
  , carRR, carCC, cdrRR, cdrCC, carRS, carCS, cdrRS, cdrCS
  , setCarRSS, setCdrRSS

    -- * Test for immutable data
  , isImmutablePair, isImmutable

    -- * Equality testing
  , equalSSH

    -- * Showing 'Val' in IO
  , showValIO
  ) where

import Types
import EvaluationMonad
import Control.Monad (join, when)
import Control.Monad.Loops (allM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.IORef (readIORef) -- for show in IO
import Data.Foldable (foldrM)

import qualified Data.Array as A

makePrimitive :: Primitive -> Val
makePrimitive (Prim name arity func) = Primitive arity func name

{- Note: [List Operation Efficiency]
There's some low-hanging fruit here for optimizing list operations.
Primarily, most operations end up walking the structure more than once.
'testCircularList' walks the whole list (in the event that the list is
not circular) since it terminates when it sees a cdr that isn't a Pair.
But then 'flattenFiniteList' also walks the whole list, to flatten it,
and is always called separately from 'testCircularList'. Notably,
'isListSH' currently tests, then flattens to check the ending. But then
'getList' calls 'isListSH', then flattens to get the list! So 'getList'
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
  b <- isListSH v
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

-- | Represents a (shallow copy of a) Scheme list at the Haskell level.
-- Objects inside the list can be mutated as normal but the car/cdr
-- Refs that would be needed to mutate the shape of the list itself
-- are not present. This is useful for implementing Scheme syntax;
-- for example the first thing `eval` does is freeze its input.
--
-- Invariants:
--  (1) The Haskell lists in FrozenLists are always finite (unchecked)
--  (2) FNotList never contains Nil, a PairPtr, or an IPairPtr.
--      See Note: [Freezing Nil].
data FrozenList
  = FList [Val]
  | FDottedList [Val] Val
  | FNotList Val

lintFrozenList :: FrozenList -> EM ()
lintFrozenList fl = lintAssert "FNotList contains Nil or a Pair" $ pure $
  case fl of
    FNotList Nil -> False
    FNotList PairPtr{} -> False
    FNotList IPairPtr{} -> False
    _ -> True

{- Note: [Freezing Nil]
There are multiple possible representations of any frozen list that represents
a list. True lists can be represented with either FList or FDottedList
or FNotList (where the 'Val's in the last two cases are pairs). The empty list,
'Nil', can be represented as both 'FList []' and 'FNotList Nil'.

Invariant (2) of 'FrozenList' resolve the ambiguities. All lists should be
represented as frozen lists, as expected, meaning that lists can't appear in
'FNotList'. This is perfectly sane.

However, 'Nil' is a bit of a corner case. 'Nil' is the only list that is
self-evaluating. In most cases where we freeze a list, we care more about the
fact that 'Nil' is a list than the fact that it's self-evaluating.
In fact, the only place where we _do_ care that it's self evaluating is in
'eval'! So we always freeze 'Nil' as 'FList []', and we check the special
case in 'eval'.
-}

-- | Get an immutable Haskell representation of a Scheme list.
-- This is useful for core operations like `eval`, which can evaluate
-- a Scheme list by freezing it and then evaluating the frozen list instead.
-- Attempting to freeze a circular list raises a CircularList error.
--
-- If the object is not a list, it is returned unchanged in an
-- 'FNotList'.
--
-- The 'FrozenList' is linted before it is returned, so it is not necessary
-- to immediately lint it again.
freezeList :: Val -> EM FrozenList
freezeList v = do
  requireFiniteList v
  (lst, dot) <- flattenFiniteList v
  let flist = case (lst, dot) of
        ([], Nil) -> FList []
        ([], obj) -> FNotList obj
        (xs, Nil) -> FList xs
        (xs, obj) -> FDottedList xs obj
  lintFrozenList flist
  return flist

-- | Require that a Scheme value is not a circular list. 
-- Circular data structures that are not lists may or may not be detected. 
-- If the value is a circular list, a CircularList error is raised.
requireFiniteList :: Val -> EM ()
requireFiniteList v = whenM (testCircularList v) (throwError CircularList)

-- | Test if a Scheme value is a circular list.
testCircularList :: Val -> EM Bool
testCircularList v = case nextS v of
    Nothing   -> pure False
    Just node -> lift2 go (pure $ Just node) (nextR node)
  where
    -- Floyd's Algorithm
    go tortoise hare
      | Just t <- tortoise
      , Just h <- hare
      = if t == h
        then return True
        else lift2 go (nextR t) (next2 h)
      | otherwise = return False

    -- takes a ref to a pair obj (mut. or immut. via Either)
    -- and gets Just a ref to the cdr PairObj if the cdr is a pair,
    -- otherwise Nothing.
    nextR (Right pair) = do
      cdrR <- cdrRR pair
      val <- readRef cdrR
      return $ nextS val
    nextR (Left pair) =
      let cdrC = cdrCC pair
          ConstRef val = cdrC
      in pure $ nextS val
    
    nextS (PairPtr r) = Just (Right r)
    nextS (IPairPtr cr) = Just (Left cr)
    nextS _ = Nothing

    next2 = runMaybeT . (nextR' >=> nextR')
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
flattenFiniteList :: Val -> EM ([Val], Val)
flattenFiniteList (PairPtr pair) = do
  hd <- carRS pair
  ~(tl, dot) <- cdrRS pair >>= flattenFiniteList
  return (hd:tl, dot)
flattenFiniteList (IPairPtr cr) = do
  car <- carCS cr -- use these instead of pattern matching for linting
  cdr <- cdrCS cr
  ~(tl, dot) <- flattenFiniteList cdr
  return (car:tl, dot)

flattenFiniteList other = return ([], other)

-- | Make a freshly-allocated mutable Scheme list containing
-- the given values.
makeMutableList :: [Val] -> EM Val
makeMutableList = foldrM consSSS Nil

-- | Make a freshly-allocated, mutable, improper Scheme list containing
-- the given values.
makeImproperMutableList :: [Val] -- ^ the "proper" part of the list
                        -> Val   -- ^ the improper tail
                        -> EM Val
makeImproperMutableList prop improp = foldrM consSSS improp prop

-- | Make a freshly-allocated _immutable_ Scheme list containing
-- the given values.
--
-- Unless you're working on the parser, you want 'makeMutableList'
-- instead of this.
makeImmutableList :: [Val] -> Val
makeImmutableList = foldr immutableCons Nil

-- | Make a freshly allocated, immutable, improper Scheme list.
--
-- You probably don't actually want this function.
-- See 'makeImproperMutableList' and 'makeImmutableList'.
makeImproperImmutableList :: [Val] -> Val -> Val
makeImproperImmutableList prop improp = foldr immutableCons improp prop

-- | Primitive list? operation.
--
-- See page 42 of the r7rs report:
-- "By definition, all lists have finite length 
-- and are terminated by the empty list."
isListSH :: Val -> EM Bool
isListSH v = callCC $ \exit -> do
  whenM (testCircularList v) $ exit False
  (_, tl) <- flattenFiniteList v
  case tl of
    Nil -> return True
    _   -> return False

-- | Primitive cons operation.
consSSS :: Val -> Val -> EM Val
consSSS car cdr = do
  pair <- PairObj <$> newRef car <*> newRef cdr
  PairPtr <$> newRef pair

-- | Primitive cons operation for immutable lists.
--
-- In an ideal world, this function would perform linting to check that
-- the car and cdr are immutable objects. Unfortunately, that would require
-- access to the repl 'Opts' in the parser (the only place this function
-- should be used) which are currently (12/10/2020) not available. As the
-- language grows, they will probably become available eventually, at which
-- point this unfortunate issue should be patched.
--
-- Unless you're working on the parser, you really shouldn't use this function!
immutableCons :: Val -> Val -> Val
immutableCons car cdr =
  IPairPtr $ ConstRef $ IPairObj (ConstRef car) (ConstRef cdr)

carRR :: MonadIO m => Ref PairObj -> m (Ref Val)
carRR pair = do
  PairObj c _d <- liftIO $ readIORef pair
  return c

carCC :: ConstRef IPairObj -> ConstRef Val
carCC (ConstRef (IPairObj c _d)) = c

cdrRR :: MonadIO m => Ref PairObj -> m (Ref Val)
cdrRR pair = do
  PairObj _c d <- liftIO $ readIORef pair
  return d

cdrCC :: ConstRef IPairObj -> ConstRef Val
cdrCC (ConstRef (IPairObj _c d)) = d

carRS :: MonadIO m => Ref PairObj -> m Val
carRS ref = carRR ref >>= liftIO . readIORef

carCS :: ConstRef IPairObj -> EM Val
carCS cr = do lintAssert "carCS: car is mutable" (pure $ isImmutable v)
              return v
  where ConstRef v = carCC cr

cdrRS :: MonadIO m => Ref PairObj -> m Val
cdrRS ref = cdrRR ref >>= liftIO . readIORef

cdrCS :: ConstRef IPairObj -> EM Val
cdrCS cr = do lintAssert "cdrCS: cdr is mutable" (pure $ isImmutable v)
              return v 
  where ConstRef v = cdrCC cr

setCarRSS :: Ref PairObj -> Val -> EM Val
setCarRSS pair v = do
  carRef <- carRR pair
  writeRef carRef v
  return v

setCdrRSS :: Ref PairObj -> Val -> EM Val
setCdrRSS pair v = do
  cdrRef <- cdrRR pair
  writeRef cdrRef v
  return v

-- | Require that the given Val is a list. Otherwise, raises a 'TypeError'.
requireList :: Val -> EM ()
requireList v =
  whenM (not <$> isListSH v) $ throwError $ TypeMismatch "list" v

whenM :: Monad m => m Bool -> m () -> m ()
whenM test thing = do
  b <- test
  when b thing

-- | Test if a given Scheme object is an immutable pair.
isImmutablePair :: Val -> Bool
isImmutablePair IPairPtr{} = True
isImmutablePair _ = False

-- | Test if a given Scheme object is immutable.
--
-- Specifically, we mean that an object is "immutable" if it can be output
-- by the parser. That means the type is not a reference, or its constructor's
-- name starts with I.
--
-- Most objects qualify; 'PairPtr', 'VecPtr', 'U8VecPtr', and 'StringPtr' do
-- not.
isImmutable :: Val -> Bool
isImmutable PairPtr{} = False
isImmutable _ = True


-- TODO [r7rs]
-- This implementation of 'equalSSH':
--  (1) loops on cyclic data
--  (2) can't compare closures/continuations
--      because we aren't tagging them

-- | Test if two given Scheme objects are equal, in the sense of
-- r7rs' equal? procedure.
equalSSH :: Val -> Val -> EM Bool
equalSSH (Atom i) (Atom j) = pure $ i == j
equalSSH (Vector v1) (Vector v2) =
  if A.bounds v1 /= A.bounds v2
    then pure False
    else do
      let l1 = A.elems v1
          l2 = A.elems v2
          r = zipWith equalSSH l1 l2
      allM id r
equalSSH (Number i) (Number j) = pure $ i == j
equalSSH (String s) (String t) = pure $ s == t
equalSSH (Char c1) (Char c2) = pure $ c1 == c2
equalSSH (Bool b1) (Bool b2) = pure $ b1 == b2
equalSSH (Primitive _ _ n1) (Primitive _ _ n2) =
  pure $ n1 == n2
equalSSH (PrimMacro _ _ n1) (PrimMacro _ _ n2) =
  pure $ n1 == n2
equalSSH Continuation{} _ = pure False
equalSSH _ Continuation{} = pure False
equalSSH Closure{} _ = pure False
equalSSH _ Closure{} = pure False
equalSSH (Port h1) (Port h2) = pure $ h1 == h2
equalSSH Undefined _ = panic "equalSSH: #<undefined>"
equalSSH _ Undefined = panic "equalSSH: #<undefined>"
equalSSH Nil Nil = pure True
equalSSH (PairPtr r1) (PairPtr r2) = do
  let carEqual = equalSSH <$> carRS r1 <*> carRS r2
      cdrEqual = equalSSH <$> cdrRS r1 <*> cdrRS r2
  allM id [join carEqual, join cdrEqual]
equalSSH (IPairPtr r1) (IPairPtr r2) = do
  let carEqual = equalSSH <$> carCS r1 <*> carCS r2
      cdrEqual = equalSSH <$> cdrCS r1 <*> cdrCS r2
  allM id [join carEqual, join cdrEqual]


-- | Show a 'Val' in IO. This function loops forever
-- on cyclic data; it's morally equivalent to write-simple.
-- However we don't use it to _implement_ write-simple
-- because we can 't do the linting that we want to do
-- since we aren't in EM. This is mostly just a temporary
-- (12/10/2020) thing so that we can look at values until
-- write-simple/write-shared are implemented. Once we have
-- those, it's easy enough to implement
-- 'unsafeEMtoIO :: EM a -> IO a' using 'execAnyEM'.
showValIO :: Val -> IO String
showValIO = fmap ($ "") . showsValIO

-- | ShowS version of 'showValIO'.
showsValIO :: Val -> IO ShowS
showsValIO p@PairPtr{}  = showParen True <$> showsListIO p
showsValIO p@IPairPtr{} = showParen True <$> showsListIO p     
showsValIO s = pure $ shows s

showsListIO :: Val -> IO ShowS
-- only accepts pairs
showsListIO (PairPtr r) = do
  car <- carRS r
  showsCar <- showsValIO car
  (showsCar .) <$> (cdrRS r >>= showsListIO1)
-- we're in IO, so have to forego linting here.
showsListIO (IPairPtr (ConstRef ipo)) = case ipo of
  IPairObj (ConstRef car) (ConstRef cdr) -> do
    showsCar <- showsValIO car
    (showsCar .) <$> showsListIO1 cdr

-- | 'showsListIO', but puts a space before showing
-- the next object. If the next object is 'Nil' (i.e., we're done)
-- then no space is printed.
--
-- This is suitable for printing objects in lists other than the first.
showsListIO1 :: Val -> IO ShowS
showsListIO1 p@PairPtr{} = (showString " " .) <$> showsListIO p
showsListIO1 p@IPairPtr{} = (showString " " .) <$> showsListIO p
showsListIO1 Nil = pure id
showsListIO1 dot = (showString " . " .) <$> showsValIO dot
