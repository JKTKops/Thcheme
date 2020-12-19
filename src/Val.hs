module Val
  ( -- * Val and support types
    Val (..)
  , LispErr (..), isTerminationError
  , Primitive (..)
  , Macro (..)
  , Builtin, InTail --, Arity (..)

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
  , carPR, cdrPR, carPS, cdrPS
  , setCarSSS, setCdrSSS, pairSH

    -- * Vectors
  , vectorSH

    -- * Test for immutable data
  , isImmutablePair, isImmutable

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

import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import System.Mem.StableName (makeStableName)

makePrimitive :: Primitive -> Val
makePrimitive (Prim name arity func) = Primitive arity func name

-- | Is this 'Val' truthy?
truthy :: Val -> Bool
truthy (Bool False) = False
truthy _ = True

-------------------------------------------------------------------------------
--
-- Pure 'Val' instances. You probably don't want to use these, but they are
-- ocassionally useful in the test suite etc. Be careful!
--
-------------------------------------------------------------------------------


showVal :: Val -> ShowS
showVal (Atom s) = showString s
showVal (Number n) = shows n
showVal (String s) = shows s
showVal (Char c)   = showString "#\\" . showString (case c of
    ' '  -> "space"
    '\t' -> "tab"
    '\n' -> "newline"
    '\r' -> "carriage-return"
    _    -> [c])
showVal (Bool True) = showString "#t"
showVal (Bool False) = showString "#f"
showVal Port{} = showString "#<port>"
showVal Undefined = showString "#<undefined>"
showVal (Primitive _ _ name) = showString $ "#<function " ++ name ++ ">"
showVal Continuation{} = showString "#<cont>"
showVal (Closure args varargs body env name) = 
    showParen True $
        displayName . showChar ' '
      . showParen True (displayArgs . displayVarargs)
      . showString " ..."
  where
    displayName = showString $ fromMaybe "lambda" name
    displayArgs = showString $ unwords args
    displayVarargs = case varargs of
        Nothing  -> id
        Just arg -> showString " . " . showString arg
showVal (PrimMacro _ _ name) = showString $ "#<macro " ++ name ++ ">"
showVal Pair{} = showString "<can't show mutable pair>"
showVal p@IPair{} = showParen True $ showDList $ fromList p
  where
    fromList :: Val -> ([Val], Val)
    fromList (IPair c d) =
      first (c:) $ fromList d
    fromList obj = ([], obj)
    
    first f (a, b) = (f a, b)

    showDList (vs, v) = unwordsList vs . case v of
        Nil -> id
        obj -> showString " . " . shows obj

showVal Vector{} = showString "<can't show mutable vector>"
showVal (IVector v) = showChar '#' . showParen True (unwordsList (V.toList v))
showVal Nil = showString "()"

-- | Can't show mutable things.
instance Show Val where
    showsPrec _ v s = showVal v s

showErr :: LispErr -> String
showErr (UnboundVar message varname)  = message ++ " unbound symbol: " ++ varname
showErr (EvaluateDuringInit name) = name ++ " referred to itself during initialization"
showErr (SetImmutable tyname) = "can't set immutable " ++ tyname
showErr (BadSpecialForm form) = "bad form: " ++ show form
showErr (NotFunction message func)    = message ++ ": " ++ show func
showErr (NumArgs expected found)      = "expected " ++ show expected
    ++ " arg" ++ (if expected == 1
        then ""
        else "s")
    ++ "; found values " ++ show found
showErr (TypeMismatch expected found) = "invalid type: expected " ++ expected
    ++ ", found " ++ show found
showErr CircularList                  = "circular list"
showErr EmptyBody                     = "attempt to define function with no body"
showErr (Parser parseErr)             = "parse error at " ++ show parseErr
showErr (Default message)             = message
showErr Quit                          = "quit invoked"

-- | This instance can't show internal values with mutable pairs and will
-- likely be removed soon instead of maintaining it. If you want to turn an
-- error into a string, you should use 'showErrIO' in Primitives.WriteLib.
instance Show LispErr where show = ("Error: " ++ ) . showErr

intercalateS :: String -> [ShowS] -> ShowS
intercalateS sep = go
  where go []     = id
        go [s]    = s
        go (s:ss) = s . showString sep . go ss

unwordsList :: [Val] -> ShowS
unwordsList = intercalateS " " . map shows

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
lintFrozenList fl = lintAssert "FNotList contains Nil or a pair" $ pure $
  case fl of
    FNotList Nil     -> False
    FNotList Pair{}  -> False
    FNotList IPair{} -> False
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
testCircularList v = next v >>= \case
    Nothing   -> pure False
    Just node -> lift2 go (pure $ Just node) (next node)
  where
    -- Floyd's Algorithm
    go :: Maybe Val -> Maybe Val -> EM Bool
    go tortoise hare
      | Just t <- tortoise
      , Just h <- hare
      = ifM ((==) <$> liftIO (makeStableName t) <*> liftIO (makeStableName h))
            (return True)
            (lift2 go (next t) (next2 h))
      | otherwise = return False

    -- takes a ref to a pair obj (mut. or immut. via Either)
    -- and gets Just a ref to the cdr PairObj if the cdr is a pair,
    -- otherwise Nothing.
    next p@Pair{}  = Just <$> cdrPS p
    next p@IPair{} = Just <$> cdrPS p
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
flattenFiniteList p@Pair{} = do
  hd <- carPS p
  ~(tl, dot) <- cdrPS p >>= flattenFiniteList
  return (hd:tl, dot)
flattenFiniteList p@IPair{} = do
  car <- carPS p
  cdr <- cdrPS p
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
consSSS car cdr = Pair <$> newRef car <*> newRef cdr

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
immutableCons = IPair

carPR :: Val -> Ref Val
carPR (Pair c _d) = c
carPR _ = panic "carPR: not a mutable pair"

cdrPR :: Val -> Ref Val
cdrPR (Pair _c d) = d
cdrPR _ = panic "cdrPR: not a mutable pair"

carPS :: MonadIO m => Val -> m Val
carPS p@Pair{} = liftIO $ readIORef $ carPR p
carPS (IPair car _) = pure car
carPS _ = panic "carPS: not a pair"

cdrPS :: MonadIO m => Val -> m Val
cdrPS p@Pair{} = liftIO $ readIORef $ cdrPR p
cdrPS (IPair _ cdr) = pure cdr
cdrPS _ = panic "cdrPS: not a pair"

setCarSSS :: Val -> Val -> EM Val
setCarSSS (Pair c _d) v = do
  writeRef c v
  return v
setCarSSS IPair{} _ = throwError $ SetImmutable "pair"
setCarSSS v _ = throwError $ TypeMismatch "pair" v

setCdrSSS :: Val -> Val -> EM Val
setCdrSSS (Pair _c d) v = do
  writeRef d v
  return v
setCdrSSS IPair{} _ = throwError $ SetImmutable "pair"
setCdrSSS v _ = throwError $ TypeMismatch "pair" v

-- | Require that the given Val is a list. Otherwise, raises a 'TypeError'.
requireList :: Val -> EM ()
requireList v =
  whenM (not <$> isListSH v) $ throwError $ TypeMismatch "list" v

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = b >>= \b' -> if b' then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM test thing = ifM test thing (pure ())

-- | Test if a given Scheme object is an immutable pair.
isImmutablePair :: Val -> Bool
isImmutablePair IPair{} = True
isImmutablePair _ = False

-- | Test if a given Scheme object is immutable.
--
-- Specifically, we mean that an object is "immutable" if it can be output
-- by the parser. That means the type is not a reference, or its constructor's
-- name starts with I.
--
-- Most objects qualify; 'Pair', 'Vector', 'U8Vector', and 'String' do
-- not.
isImmutable :: Val -> Bool
isImmutable Pair{} = False
isImmutable Vector{} = False
isImmutable _ = True

pairSH :: Val -> Bool
pairSH Pair{}  = True
pairSH IPair{} = True
pairSH _ = False

vectorSH :: Val -> Bool
vectorSH Vector{} = True
vectorSH IVector{} = True
vectorSH _ = False

-- | Show a 'Val' in IO. This function loops forever
-- on cyclic data; it's morally equivalent to write-simple.
-- However we don't use it to _implement_ write-simple
-- because we can 't do the linting that we want to do
-- since we aren't in EM. This is mostly just a temporary
-- (12/10/2020) thing so that we can look at values until
-- write-simple/write-shared are implemented (in 'MonadIO',
-- preferably).
--
-- TODO: it should be good to get rid of this now.
showValIO :: Val -> IO String
showValIO = fmap ($ "") . showsValIO

-- | ShowS version of 'showValIO'.
showsValIO :: Val -> IO ShowS
showsValIO p@Pair{}  = showParen True <$> showsListIO p
showsValIO p@IPair{} = showParen True <$> showsListIO p     
showsValIO s = pure $ shows s

showsListIO :: Val -> IO ShowS
-- only accepts pairs
showsListIO (Pair c d) = do
  car <- readIORef c
  showsCar <- showsValIO car
  (showsCar .) <$> (readIORef d >>= showsListIO1)
-- we're in IO, so have to forego linting here.
showsListIO (IPair car cdr) = do
    showsCar <- showsValIO car
    (showsCar .) <$> showsListIO1 cdr

-- | 'showsListIO', but puts a space before showing
-- the next object. If the next object is 'Nil' (i.e., we're done)
-- then no space is printed.
--
-- This is suitable for printing objects in lists other than the first.
showsListIO1 :: Val -> IO ShowS
showsListIO1 p@Pair{}  = (showString " " .) <$> showsListIO p
showsListIO1 p@IPair{} = (showString " " .) <$> showsListIO p
showsListIO1 Nil = pure id
showsListIO1 dot = (showString " . " .) <$> showsValIO dot
