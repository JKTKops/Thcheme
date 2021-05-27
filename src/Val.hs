{-# LANGUAGE RankNTypes #-}

module Val
  ( -- * Val and support types
    Val (..), Number (..), RealNumber (..)
  , LispErr (..), isTerminationError
  , Primitive (..)
  , Macro (..)
  , Builtin, InTail, Arity (..)

    -- * Utilities for 'Val's
  , truthy, isConstant
  , makePrimitive
  , showArity, usePluralForArity, testArity

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
  , makeVector, makeByteVector

    -- * Test for immutable data
  , isImmutablePair, isImmutable

    -- * Making and working with Numbers
  , makeBignum, makeFlonum, makeRatnum
  , approxToRatnum, realInexact
  , getExactInteger, isExactZero
  , numberSH, complexSH, realSH, rationalSH, integerSH
  , implicitRealConversion, implicitNumericConversion

    -- * Showing 'Val' in IO
  , showValIO
  ) where

import Types
import EvaluationMonad
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bits (shiftL, bit)
import Data.Complex (Complex((:+)), magnitude)
import Data.IORef (readIORef) -- for show in IO
import Data.Foldable (foldrM)
import Data.Ratio ((%), numerator, denominator)
import Data.Word (Word8)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Maybe (fromMaybe)
import System.Mem.StableName (makeStableName)

import GHC.Float (Floating(..))

makePrimitive :: Primitive -> Val
makePrimitive (Prim name arity func) = Primitive arity func name

-- | Is this 'Val' truthy?
truthy :: Val -> Bool
truthy (Bool False) = False
truthy _ = True

-- | Is this 'Val' an atomic constant?
isConstant :: Val -> Bool
isConstant Number{} = True
isConstant Bool{}   = True
isConstant Char{}   = True
isConstant Nil{}    = True
isConstant _        = False

-------------------------------------------------------------------------------
--
-- Pure 'Val' instances. You probably don't want to use these, but they are
-- ocassionally useful in the test suite etc. Be careful!
--
-------------------------------------------------------------------------------

-- | 'show' a 'Val'. This almost certainly isn't what you want;
-- it's a good escape hatch if you need a pure way to display a 'Val'
-- but it doesn't display unicode characters in strings properly,
-- can't display anything mutable, and loops on cyclic immutable data.
showVal :: Val -> ShowS
showVal (Symbol s) = showString s
showVal (Number n) = shows n
showVal (String _s) = showString "<can't show mutable string>"
showVal (IString s) = shows s
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
showVal (Primitive arity _ name) = shows fakeClosure
  where
    fakeClosure = Closure fakeArgs fakeVarargs [] [] (Just name)
    (fakeArgs, fakeVarargs) = case arity of
      Exactly n     -> (replicate n "_", Nothing)
      Between lo hi -> (fakeBetween lo hi, Nothing)
      AtLeast n     -> (replicate n "_", Just "_")
    fakeBetween lo hi =
      let reqs = replicate lo "_"
          numOpts = hi - lo
          opts = replicate numOpts "[_]"
      in reqs ++ opts

showVal Continuation{} = showString "#<cont>"
showVal (Closure [] (Just varargs) _body _env name) =
      showString "#<procedure "
    . showString (fromMaybe "lambda" name)
    . showChar ' ' . showString varargs
    . showChar '>' 
showVal (Closure args varargs _body _env name) = 
      showString "#<procedure "
      . displayName . showChar ' '
      . showParen True (displayArgs . displayVarargs)
      . showChar '>'
  where
    displayName = showString $ fromMaybe "lambda" name
    displayArgs = showString $ unwords args
    displayVarargs = case varargs of
        Nothing  -> id
        Just arg -> showString " . " . showString arg
showVal (PrimMacro _ _ name) = showString $ "#<macro " ++ name ++ ">"
showVal (MacroTransformer mname _) = showString $ "#<macro" ++ name ++ ">"
  where name = case mname of
                 Nothing -> ""
                 Just name -> " " ++ name
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
showVal (IByteVector v) = showString "#u8" . showParen True (showBytes (U.toList v))
  where showBytes [] = id
        showBytes [b] = shows b
        showBytes (b:bs) = shows b . showChar ' ' . showBytes bs
showVal Nil = showString "()"
showVal MultipleValues{} = error "panic! MultipleValues in showVal"

-- | This instance can't show mutable things.
instance Show Val where
    showsPrec _ v s = showVal v s

-- | Doesn't display exactness indicators and always uses base 10.
instance Show Number where
  show (Real r) = show r
  show (Complex (r :+ i)) = show r ++ sign ++ show i ++ "i"
    where sign = if i < 0 then "" else "+"

showErr :: LispErr -> String
showErr (UnboundVar message varname)  = message ++ " unbound symbol: " ++ varname
showErr (EvaluateDuringInit name) = name ++ " referred to itself during initialization"
showErr (SetImmutable tyname) = "can't set immutable " ++ tyname
showErr (BadSpecialForm form) = "bad form: " ++ show form
showErr (NotFunction message func)    = message ++ ": " ++ show func
showErr (NumArgs expected found)      = "expected " ++ showArity expected
    ++ " arg" ++ (if usePluralForArity expected then "s" else "")
    ++ ", found values " ++ show found
showErr (TypeMismatch expected found) = "invalid type: expected " ++ expected
    ++ ", found " ++ show found
showErr CircularList                  = "circular list"
showErr EmptyBody                     = "attempt to define function with no body"
showErr (Parser parseErr)             = parseErr
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

showArity :: Arity -> String
showArity (Exactly n) = show n
showArity (AtLeast n) = "at least " ++ show n
showArity (Between i j) = unwords ["between", show i, "and", show j]

usePluralForArity :: Arity -> Bool
usePluralForArity (Exactly 1) = False
usePluralForArity (AtLeast 1) = False
usePluralForArity _ = True

-- | Test an 'Arity' against the length of a list, reporting an error
-- if the arity is not satisfied.
--
-- More efficient than using 'length'.
testArity :: Arity -> [Val] -> EM ()
testArity (Exactly n0) vs0
  | n0 < 0 = panic "negative exact arity"
  | otherwise = check $ cmpLength n0 vs0
  where
    check EQ = pure ()
    check _  = throwError $ NumArgs (Exactly n0) vs0
testArity (AtLeast n0) vs0 = check $ cmpLength n0 vs0
  where
    check GT = throwError $ NumArgs (AtLeast n0) vs0
    check _  = pure ()
testArity a@(Between lo hi) vs0 = check (cmpLength lo vs0) (cmpLength hi vs0)
  where
    check GT _ = raise
    check _ LT = raise
    check _ _  = pure ()
    raise = throwError $ NumArgs a vs0

cmpLength :: Int -> [a] -> Ordering
cmpLength 0 [] = EQ
cmpLength _ [] = GT
cmpLength 0 (_:_) = LT
cmpLength n (_:vs) = cmpLength (n-1) vs

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
--
--  (1) The Haskell lists in FrozenLists are always finite (unchecked)
--  (2) FNotList never contains 'Nil', a 'Pair', or an 'IPair'.
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

    -- takes a val and returns the cdr if it is a pair, else Nothing
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
isImmutable Pair{}   = False
isImmutable Vector{} = False
isImmutable String{} = False
isImmutable _ = True

makeBignum :: Integer -> Val
makeBignum = Number . Real . Bignum

makeFlonum :: Double -> Val
makeFlonum = Number . Real . Flonum

makeRatnum :: Integer -> Integer -> Val
makeRatnum x y = Number $ Real $ Ratnum (x % y)

approxToRatnum :: Double -> Val
approxToRatnum = Number . Real . Ratnum . toRational

getExactInteger :: Val -> Maybe Integer
getExactInteger v = case v of
  Number (Real (Bignum i)) -> return i
  Number (Real (Flonum _)) -> Nothing
  Number (Real (Ratnum r)) -> case denominator r of
    1 -> return $ numerator r
    _ -> Nothing
  Number (Complex (r :+ i)) -> case i of
    Bignum 0 -> getExactInteger (Number (Real r))
    Flonum _ -> Nothing
    Ratnum z -> if z == 0 then getExactInteger (Number (Real r)) else Nothing
    _notNplusZeroI -> Nothing
  _other -> Nothing

mkNumericTypecheck :: (Number -> Bool) -> Val -> Bool
mkNumericTypecheck p = f
  where f (Number n) = p n
        f _notNumber = False
{-# INLINE mkNumericTypecheck #-}

numberSH, complexSH, realSH, rationalSH, integerSH :: Val -> Bool
numberSH = mkNumericTypecheck $ const True
complexSH = numberSH -- see note on p35 of r7rs paper
realSH = mkNumericTypecheck $ \case
  Real{} -> True
  Complex (_r :+ i)
    | i == 0    -> True
    | otherwise -> False
rationalSH = mkNumericTypecheck test
  where
    test (Real (Flonum f))
      | isInfinite f || isNaN f = False
    test Real{} = True -- truncated real numbers are always rational!
    test (Complex (r :+ i))
      | i == 0 = test $ Real r
      | otherwise = False
integerSH = mkNumericTypecheck test
  where
    test (Real x) = x == round x -- (==)@RealNumber is Scheme =
    test (Complex (r :+ i))
      | i == 0 = test $ Real r
      | otherwise = False

implicitNumericConversion :: Number -> Number -> (Number, Number)
implicitNumericConversion (Real x) (Real y) = (Real x, Real y)
implicitNumericConversion x y = (asComplex x, asComplex y)
  where
    asComplex :: Number -> Number
    asComplex (Real r) = Complex (r :+ 0)
    asComplex (Complex c) = Complex c

-- | Perform an implicit Scheme conversion of two numbers.
--
-- The "order" of numeric types is bignum < ratnum < flonum.
-- The lesser argument will be casted upwards to match the greater one.
-- The returned Numbers are guaranteed to use the same Number constructor.
implicitRealConversion :: RealNumber -> RealNumber -> (RealNumber, RealNumber)
implicitRealConversion = work
  where
    work x@Flonum{} y = (x, Flonum $ asFlonum y)
    work x y@Flonum{} = (Flonum $ asFlonum x, y)
    work x@Ratnum{} y = (x, asRatnum y)
    work x y@Ratnum{} = (asRatnum x, y)
    work x y = (x, y) -- both Bignum

    asRatnum (Ratnum r) = Ratnum r
    asRatnum (Bignum n) = Ratnum (n % 1)
    asRatnum Flonum{} = error "implicitRealConversion.asRatnum: impossible flonum"

pairSH :: Val -> Bool
pairSH Pair{}  = True
pairSH IPair{} = True
pairSH _ = False

vectorSH :: Val -> Bool
vectorSH Vector{} = True
vectorSH IVector{} = True
vectorSH _ = False

makeVector :: [Val] -> Val
makeVector = IVector . V.fromList

makeByteVector :: [Word8] -> Val
makeByteVector = IByteVector . U.fromList

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
showsListIO _notPair = error "showsListIO: not a list"

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

-------------------------------------------------------------------------------
-- Numeric Class Instances for Number
-------------------------------------------------------------------------------

type Binop a = a -> a -> a
type Monop a = a -> a

{- HLINT ignore "Redundant lambda" -}
mkRealNumberBinop :: (forall a. Num a => Binop a)
                  -> Binop RealNumber
mkRealNumberBinop op = \x y -> case implicitRealConversion x y of
  (Bignum a, Bignum b) -> Bignum (a `op` b)
  (Ratnum a, Ratnum b) -> Ratnum (a `op` b)
  (Flonum a, Flonum b) -> Flonum (a `op` b)
  _ -> error "mkRealNumberBinop: implicitRealConversion failure"
{-# INLINE mkRealNumberBinop #-}

{- HLINT ignore "Redundant lambda" -}
mkRealNumberMonop :: (forall a. Num a => Monop a)
                  -> Monop RealNumber
mkRealNumberMonop op = \case
  Bignum a -> Bignum (op a)
  Ratnum a -> Ratnum (op a)
  Flonum a -> Flonum (op a)
{-# INLINE mkRealNumberMonop #-}

{- HLINT ignore "Redundant lambda" -}
mkNumberBinop :: (forall a. Num a => Binop a)
              -> Binop Number
mkNumberBinop op = \x y -> case implicitNumericConversion x y of
  (Real a, Real b) -> Real (a `op` b)
  (Complex w, Complex z) -> Complex (w `op` z)
  _ -> panic " mkNumberBinop: implicitNumericConversion failure"
{-# INLINE mkNumberBinop #-}

{- HLINT ignore "Redundant lambda" -}
mkNumberMonop :: (forall a. Num a => Monop a)
              -> Monop Number
mkNumberMonop op = \case
  Real n -> Real $ op n
  Complex c -> Complex $ op c
{-# INLINE mkNumberMonop #-}

realNumberPlus, realNumberSub, realNumberTimes :: Binop RealNumber
realNumberPlus  = mkRealNumberBinop (+)
realNumberSub   = mkRealNumberBinop (-)
realNumberTimes x y
  | isExactZero (Real x) || isExactZero (Real y) = Bignum 0
  | otherwise = mkRealNumberBinop (*) x y

realNumberNegate, realNumberAbs, realNumberSignum :: Monop RealNumber
realNumberNegate = mkRealNumberMonop negate
realNumberAbs    = mkRealNumberMonop abs
realNumberSignum = mkRealNumberMonop signum

numberTimes :: Binop Number
numberTimes x y
  | isExactZero x || isExactZero y = 0
  | otherwise = mkNumberBinop (*) x y

isExactZero :: Number -> Bool
isExactZero (Real (Bignum 0)) = True
isExactZero (Real (Ratnum 0)) = True
isExactZero (Complex (r :+ i)) = isExactZero (Real r) && isExactZero (Real i)
isExactZero _          = False
{-# INLINABLE isExactZero #-}

instance Num RealNumber where
  (+) = realNumberPlus
  (-) = realNumberSub
  (*) = realNumberTimes
  negate = realNumberNegate
  abs    = realNumberAbs
  signum = realNumberSignum
  fromInteger = Bignum

instance Num Number where
  (+) = mkNumberBinop (+)
  (-) = mkNumberBinop (-)
  (*) = numberTimes -- checks for exact zeroes
  negate = mkNumberMonop negate
  abs    = mkNumberMonop abs
  signum = mkNumberMonop signum
  fromInteger = Real . Bignum

asFlonum :: RealNumber -> Double
asFlonum (Bignum i) = fromInteger i
asFlonum (Ratnum r) = fromRational r
asFlonum (Flonum f) = f

realInexact :: RealNumber -> RealNumber
realInexact = Flonum . asFlonum

flonumPredicate :: (Double -> Bool) -> RealNumber -> Bool
flonumPredicate p = applyPred
  where applyPred (Flonum f) = p f
        applyPred _notFlonum = False
{-# INLINE flonumPredicate #-}

-- | Implementation of 'scaleFloat' for Number. Does the right thing
-- without converting where possible. Bignums are converted to Ratnums
-- if the power is negative.
--
-- realNumberScale p n returns 0 if p is negative and n is 0. This is the
-- same behavior as scaleFloat for Float/Double and gives correct
-- results for Complex divison.
realNumberScale :: Int -> RealNumber -> RealNumber
realNumberScale p (Flonum f) = Flonum (scaleFloat p f)
realNumberScale p bigOrRat
  | p < 0 = case bigOrRat of
    Bignum 0 -> Bignum 0
    Ratnum 0 -> Bignum 0 -- simplify. No cost to doing this because a
                         -- rational would need to be deconstructed to do
                         -- the next step anyway.
    _notZero -> bigOrRat * recip (Bignum (bit $ negate p))
  | otherwise = case bigOrRat of
    Bignum i -> Bignum (i `shiftL` p)
    Ratnum r -> Ratnum (r * (2 ^ p))
    Flonum{} -> error "realNumberScale: impossible flonum"

instance RealFloat RealNumber where
  floatRadix  = floatRadix  . asFlonum
  floatDigits = floatDigits . asFlonum
  floatRange  = floatRange  . asFlonum
  decodeFloat = decodeFloat . asFlonum
  encodeFloat s e = Flonum (encodeFloat s e)
  exponent (Flonum f) = exponent f
  exponent _bigOrRat  = 0
  significand = Flonum . significand . asFlonum
  scaleFloat = realNumberScale
  isNaN = flonumPredicate isNaN
  isInfinite = flonumPredicate isInfinite
  isDenormalized = flonumPredicate isDenormalized
  isNegativeZero = flonumPredicate isNegativeZero
  -- Bignums are not IEEE floating-point numbers!
  isIEEE = flonumPredicate isIEEE
  atan2 x y = Flonum (atan2 (asFlonum x) (asFlonum y))

mkRoundingMode :: Integral b
               => (forall a. RealFrac a => a -> b)
               -> RealNumber -> b
mkRoundingMode mode = applyMode
  where applyMode (Bignum i) = fromInteger i -- no rounding to do
        applyMode (Ratnum r) = mode r
        applyMode (Flonum f) = mode f
{-# INLINE mkRoundingMode #-}

instance RealFrac RealNumber where
  properFraction (Bignum a) = (fromInteger a, 0)
  properFraction (Ratnum r) = let (n, f) = properFraction r
                              in  (n, Ratnum f)
  properFraction (Flonum f) = let (n, f') = properFraction f
                              in  (n, Flonum f')

  truncate = mkRoundingMode truncate
  round    = mkRoundingMode round
  ceiling  = mkRoundingMode ceiling
  floor    = mkRoundingMode floor

instance Real RealNumber where
  toRational (Bignum i) = i % 1
  toRational (Ratnum r) = r
  toRational (Flonum f) = toRational f

floatingOp :: Monop Double -> Monop RealNumber
floatingOp f n = Flonum (f (asFlonum n))
{-# INLINE floatingOp #-}

instance Floating RealNumber where
  pi = Flonum pi
  exp = floatingOp exp
  log = floatingOp log
  sqrt = floatingOp sqrt
  sin = floatingOp sin
  cos = floatingOp cos
  tan = floatingOp tan
  asin = floatingOp asin
  acos = floatingOp acos
  atan = floatingOp atan
  sinh = floatingOp sinh
  cosh = floatingOp cosh
  tanh = floatingOp tanh
  asinh = floatingOp asinh
  acosh = floatingOp acosh
  atanh = floatingOp atanh
  log1p = floatingOp log1p
  expm1 = floatingOp expm1
  log1pexp = floatingOp log1pexp
  log1mexp = floatingOp log1mexp

  x ** Bignum y = x ^^ y
  x ** y = Flonum (asFlonum x ** asFlonum y)
  logBase x y = Flonum (logBase (asFlonum x) (asFlonum y))

realPart :: Number -> RealNumber
realPart (Real r) = r
realPart (Complex (x:+_)) = x
inf, nan :: RealNumber
inf = Flonum $ 1/0
nan = Flonum $ 0/0

numberIsInfinite :: Number -> Bool
numberIsInfinite (Real r) = isInfinite r
numberIsInfinite (Complex (x:+y)) = isInfinite x || isInfinite y

-- | This works for sin, cos, tan, sinh, cosh, and tanh,
-- which all have real results wherever they are defined
-- on the real line.
--
-- It does not work for inverse trig functions, hence the name.
forwardTrigFunction :: (forall a. Floating a => Monop a)
                    -> Monop Number
forwardTrigFunction f = \case
  Real r    -> Real (f r)
  Complex z -> Complex (f z)

instance Floating Number where
  pi = Real pi
  
  exp (Real r) = Real (exp r)
  exp (Complex (x:+y)) = Complex $ expx * cos y :+ expx * sin y
    where expx = exp x
  
  log (Real r)
    | r < 0 || isNegativeZero r = Complex $ log (abs r) :+ pi
    | otherwise = Real $ log r
  log (Complex z@(x:+y)) = Complex $ log (magnitude z) :+ atan2 y x

  x ** y 
    | isExactZero y = Real 1
    | isExactZero x = case compare (realPart y) 0 of
      GT -> Real 0
      LT -> Real inf
      EQ -> Real nan
    | numberIsInfinite x = case compare (realPart y) 0 of
      GT -> Real inf
      LT -> Real 0
      EQ -> Real nan
      -- note that exp and log give real results when applied
      -- to real arguments, when possible.
  Real x ** Real (Bignum y) = Real (x ** Bignum y)
  Real x ** Real y | x >= 0 = Real (x ** y)
  x ** y = exp (log x * y)
  
  sqrt z | isExactZero z = 0
  sqrt (Real r)
    | r >= 0    = Real $ sqrt r
    | otherwise = Complex $ 0 :+ sqrt (-r)
  sqrt (Complex z) = Complex $ sqrt z
  
  sin = forwardTrigFunction sin
  cos = forwardTrigFunction cos
  tan = forwardTrigFunction tan

  sinh = forwardTrigFunction sinh
  cosh = forwardTrigFunction cosh
  tanh = forwardTrigFunction tanh

  -- redefine these functions to use our definition of log which behaves
  -- correctly at -0. Otherwise they are copies of Data.Complex.
  asin (Real r)
    | r >= -1 && r <= 1 = Real $ asin r
    | otherwise = asin (Complex (r :+ 0))
  asin z@(Complex (x:+y)) = Complex $ y' :+ (-x')
    where Complex (x':+y') = log (iz + sqrt (1 - z * z))
          -- take care to use log @Number and not @(Complex RealNumber)
          iz = Complex ((-y) :+ x)
  
  acos (Real r)
    | r >= -1 && r <= 1 = Real $ acos r
    | otherwise = acos (Complex (r :+ 0))
  acos z = Complex $ y'' :+ (-x'')
    where Complex (x'':+y'') = log (z + Complex ((-y'):+x'))
          Complex (x':+y')   = sqrt (1 - z * z)
  
  atan (Real r) = Real $ atan r
  atan z@(Complex (x:+y)) = Complex $ y':+(-x')
    where Complex (x':+y') = log (z' / sqrt (1 + z * z))
          z' = Complex ((1-y) :+ x)

  asinh z = log (z + sqrt (1 + z*z))
  -- see comment in Data.Complex:
  -- essentially, don't use the formula with a division because it is
  -- undefined when z = -1.
  acosh z = log (z + sqrt (z+1) * sqrt (z-1))
  atanh z = 0.5 * log ((1+z) / (1-z))

  log1p (Real r)
    | abs r < 0.5 = Real (log1p r)
    | otherwise = Real (log (1 + r))
  log1p x@(Complex (a :+ b))
    | abs a < 0.5 && abs b < 0.5 =
      let u = 2*a + a*a + b*b
      in Complex $ log1p (u/(1 + sqrt(u+1))) :+ atan2 (1+a) b
    | otherwise = log (1 + x)
  
  expm1 (Real r) = Real (expm1 r)
  expm1 x@(Complex (a :+ b))
    | a*a + b*b < 1 =
      let u = expm1 a
          v = sin (b/2)
          w = -2*v*v
      in Complex $ (u*w + u + w) :+ (u+1)*sin b
    | otherwise = exp x - 1

-- distinct from numberDiv
realNumberDivide :: Binop RealNumber
realNumberDivide x y = case implicitRealConversion x y of
  (Bignum a, Bignum b) -> Ratnum (a % b)
  (Ratnum a, Ratnum b) -> Ratnum (a / b)
  (Flonum a, Flonum b) -> Flonum (a / b)
  _ -> error "realNumberDivide: implicitRealConversion failure"

realNumberRecip :: Monop RealNumber
realNumberRecip (Bignum i) = Ratnum (1 % i)
realNumberRecip (Ratnum r) = Ratnum (recip r)
realNumberRecip (Flonum f) = Flonum (recip f)

numberDivide :: Binop Number
numberDivide x y = case implicitNumericConversion x y of
  (Real a, Real b) -> Real (a / b)
  (Complex a, Complex b) -> Complex (a / b)
  _ -> error "numberDivide: implicitNumericConversion failure"

numberRecip :: Monop Number
numberRecip (Real r)    = Real (recip r)
numberRecip (Complex z) = Complex (recip z)

instance Fractional RealNumber where
  (/) = realNumberDivide
  recip = realNumberRecip
  fromRational = Ratnum

instance Fractional Number where
  (/) = numberDivide
  recip = numberRecip
  fromRational = Real . Ratnum

realNumberCompare :: RealNumber -> RealNumber -> Ordering
realNumberCompare = (<=>) where
  Flonum f <=> Flonum g = f `compare` g
  Flonum f <=> bigOrRat
    | isInfinite f = f `compare` 0
    | otherwise    = r1 `compare` r2
    -- note that we make the fp number exact instead of making the
    -- exact number fp. This is critical. An example from the R7RS
    -- paper is: let big be (exp 2 1000). Assume big is exact and
    -- that inexact numbers are IEEE doubles [they are]. Then
    -- (= (inexact big) (+ big 1)) should be #f, but if inexact
    -- conversion is used, it will be #t.
    where r1 = toRational f
          r2 = case bigOrRat of
            Bignum i -> i % 1
            Ratnum r -> r
            Flonum{} -> error "realNumberCompare.<=>: impossible flonum"
  bigOrRat <=> n@Flonum{} = case n <=> bigOrRat of
    LT -> GT
    EQ -> EQ
    GT -> LT
  Bignum i <=> Bignum j = i `compare` j
  Bignum i <=> Ratnum r = (i % 1) `compare` r
  Ratnum r <=> Bignum i = r `compare` (i % 1)
  Ratnum r <=> Ratnum s = r `compare` s

-- | Number is not really an Integral type, since it's possible it has
-- a floating-point backing. Here's what we do:
-- 1) toInteger truncates
-- 2) quotRem uses Number division unless both arguments are Bignums.

-- This instance exists mostly so that the rounding modes from RealFrac
-- can be used with RealNumbers.
instance Integral RealNumber where
  toInteger (Bignum i) = i
  toInteger (Ratnum r) = truncate r
  toInteger (Flonum f) = truncate f

  quotRem (Bignum x) (Bignum y) = let (q, r) = quotRem x y
                                  in (Bignum q, Bignum r)
  quotRem x y = (x / y, 0)

instance Ord RealNumber where
  compare = realNumberCompare

-- | 'Enum' essentially does not have any of the laws you might expect
-- it to have; for types that are not also Bounded, it is lawless.
-- This is a useful-ish instance which generally behaves as expected
-- and which is needed to give the Integral instance.
instance Enum RealNumber where
  succ (Bignum i) = Bignum (succ i)
  succ (Ratnum r) = Ratnum (succ r)
  succ (Flonum f) = Flonum (succ f)
  pred (Bignum i) = Bignum (pred i)
  pred (Ratnum r) = Ratnum (pred r)
  pred (Flonum f) = Flonum (pred f)
  toEnum i = Bignum (toEnum i)
  fromEnum (Bignum i) = fromEnum i
  fromEnum (Ratnum r) = fromEnum r
  fromEnum (Flonum f) = fromEnum f
  -- The rest of the functions are omitted because they are not used
  -- and it is not clear what the 'correct' implementations are for
  -- Scheme Numbers.