{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Types
    ( Env, GlobalEnv, LocalEnv
    , Arity (..)
    , InTail
    , Builtin
    , Primitive (..)
    , Macro (..)
    , Val (..), Symbol, Port(..)
    , RealNumber (..), Number (..)
    , symbolName, symbolAsString
    , Ref
    , LispErr (..), ExceptionCont (..)
    , ThrowsError
    , IOThrowsError
    , extractValue
    , isTerminationError
    , liftEither
   -- , runIOThrows
    , EM (..), emCont
    , EvalState (..), dynPoint, currentHandlers
    , StackFrame (..)
    , DynamicEnv(DynamicEnv)
    , ExceptionHandler
    , DynamicPoint (..)
    , Opts
   -- , TraceType
    , liftIOThrows
    ) where

import Control.Monad.Cont
    ( (<=<),
      MonadIO(..),
      MonadCont(..),
      ContT(ContT),
      cont,
      runCont,
      Cont )
import Control.Monad.Except           ( ExceptT, MonadError(..), liftEither
                                      , runExceptT )
import Control.Monad.State.Lazy       ( MonadState(get, put), gets, modify )
import Data.Complex                   ( Complex(..) )
import Data.HashMap.Strict            ( HashMap )
import Data.IORef                     ( IORef, readIORef, writeIORef )
import Data.Ratio                     ( (%), denominator, numerator )
import Data.Text                      ( Text, unpack )
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed as U
import           Data.Word                      ( Word8 )
import           System.IO                      ( Handle )

import Options

{- Note: [IORefs in Envs]

Vals already have a notion of being "primitive" or a "pointer".
So why do 'Env's store 'IORef's? We want to be able to
_duplicate environments_ (for the purposes of capturing them in closures)
and be sure that the copied environments reflect changes in the originals.

It's not impossible that I'm completely off-base here and duplicating the
environments is not actually worth it in terms of performance. A duplicated
environment is always smaller than the original, since it flattens the
scope structure and removes shadowed bindings. However, That _usually_
isn't significantly smaller, and currently we pay an extra dereference for
every single name lookup. Low-hanging fruit?
-}
type Env = IORef (HashMap Symbol (IORef Val))
type GlobalEnv = Env
type LocalEnv  = [Env] -- ^ stack of environments forms local env
type Ref = IORef

-- * Function types and components
--type Arity = Int
data Arity
  = Exactly Int
  | AtLeast Int
  | Between Int Int
  deriving (Eq, Ord)
type InTail = Bool
type Builtin = [Val] -> EM Val
data Primitive = Prim Symbol Arity Builtin
-- The additional 'InTail' flag is necessary for macros like 'begin'
-- and 'if' which need to use a different 'eval' behavior if they are
-- in tail position. We do global CPS anyway (EM monad) but this information
-- is needed to maintain the debug stack.
data Macro = Macro Arity (InTail -> Builtin)


-- | Scheme values.
--
-- Some Scheme values can be either mutable or immutable. These are strings,
-- lists, and vectors. The immutable variant begins with I, where applicable.
--
-- The mutable data structures are interpreter-only;
-- the parser cannot construct them.
--
-- Invariant: All objects contained in an IPair or IVector are immutable.
--   Note that this is implicitly transitive.
data Val
  = Symbol Symbol

  | Number !Number

  | String !(Ref Text)
  | IString Text
  | Char Char
  | Bool Bool

  | Primitive Arity Builtin Symbol
  | PrimMacro Arity (InTail -> Builtin) Symbol
  | Continuation DynamicPoint (Val -> EM Val)
  | Closure
     { params :: [Symbol]
     , vararg :: Maybe Symbol
     , body   :: [Val]
     , cloEnv :: LocalEnv
     , name   :: Maybe Symbol
     }
  | MacroTransformer (Maybe Symbol) (Val -> EM Val)

  | Port Port

  | Nil
  | Pair !(Ref Val) !(Ref Val)
  | IPair !Val !Val

  | Vector  !(V.IOVector Val)
  | IVector !(V.Vector Val)
  | IByteVector !(U.Vector Word8)

   -- if we ever add a way to register writers for custom
   -- record types, then error objects should become a
   -- standard library record type and this should go.
  | Error Text [Val]
  | Exception LispErr

    -- | Used to identify when a recursive binding refers to itself during
    -- initialization, which is an error.
    -- If 'unspecified' or 'void' are added in the future, they should be
    -- distinct from this.
  | Undefined
    -- | Implements multiple values for the purposes of call-with-values.
    -- If one of these ever appears somewhere that it shouldn't, it will
    -- eventually get sent through `eval`, which will raise an error.
  | MultipleValues [Val]

type Symbol = Text

symbolName :: Symbol -> Text
symbolName (T.uncons -> Just ('~', name)) = name
symbolName (T.uncons -> Just ('&', name)) =
  let pieces = T.splitOn "~" name
      -- pieces are some number of components from the name
      -- if it contained tildes, followed by two components
      -- which are the unique identifiers.
  in T.intercalate "~" $ reverse $ drop 2 $ reverse pieces
symbolName primName = primName

-- | Converts a symbol to its human-readable name as a string.
-- Mostly useful for Show/write and in 'panic' messages.
symbolAsString :: Symbol -> String
symbolAsString = unpack . symbolName

data Port
  = HandlePort !Handle
  -- consuming is permanent, as indexing characters
  -- into a Unicode stream is necessarily O(n)
  -- so supporting seeking is horribly slow.
  -- When closed, the IORef is cleared so that the
  -- text can eventually be freed.
  | InputTextPort !(IORef Text) !(IORef Bool) -- open?
  -- collect chunks, which will later be collected
  -- by 'T.concat'. The chunks are stored in LIFO order.
  -- When closed, the IORef is cleared so that the
  -- list can be reclaimed by GC.
  | OutputTextPort !(IORef [Text]) !(IORef Bool) -- open?
  deriving (Eq)

-- | Scheme numbers. Note that there is no Ord Number instance;
-- complex numbers are not orderable and Scheme comparisons of
-- complex numbers are errors.
data Number
  = Real {-# UNPACK #-}!RealNumber
  | Complex {-# UNPACK #-}!(Complex RealNumber)

data RealNumber
  = Bignum {-# UNPACK #-}!Integer
  | Flonum {-# UNPACK #-}!Double
  | Ratnum {-# UNPACK #-}!Rational

-- | This is equality in the sense of Scheme '='.
instance Eq RealNumber where
  Flonum f == Flonum g = f == g
  Flonum f == bigOrRat = not (isInfinite f) && r1 == r2
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
            Flonum{} -> error "(==)@RealNumber: impossible flonum"
  bigOrRat == n@Flonum{} = n == bigOrRat
  Bignum i == Bignum j = i == j
  Bignum i == Ratnum r = i % 1 == r
  Ratnum r == Bignum i = r == i % 1
  Ratnum r == Ratnum s = r == s

instance Eq Number where
  Real a == Real b = a == b
  Real a == Complex (r :+ i) = a == r && i == Bignum 0
  Complex (r :+ i) == Real b = r == b && i == Bignum 0
  Complex (a :+ b) == Complex (x :+ y) = a == x && b == y

instance Show RealNumber where
  show (Bignum i) = show i
  show (Flonum f)
    | isInfinite f = sign : "inf.0"
    | isNaN f      = "+nan.0"
    | otherwise    = show f
    where sign = if f < 0 then '-' else '+'
  show (Ratnum r)
    | denominator r == 1 = show (numerator r)
    | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

-- | Doesn't compare mutable things; most likely you want
--
-- (1) 'eqSSH':    referential equality
-- (2) 'eqvSSH':   value equality for atoms, otherwise referential
-- (3) 'equalSSH': structural equality
eqVal :: Val -> Val -> Bool
eqVal (Symbol s) (Symbol s') = s == s'
eqVal (Number n) (Number n') = n == n'
eqVal (IString s) (IString s') = s == s'
eqVal (Char c) (Char c') = c == c'
eqVal (Bool b) (Bool b') = b == b'
eqVal (IVector v) (IVector v') = v == v'
eqVal (Port p) (Port p') = p == p'
eqVal (Primitive _ _ n) (Primitive _ _ n') = n == n'
eqVal Continuation{} _ = False -- perhaps it's possible to give continuations
eqVal _ Continuation{} = False -- unique identifiers to make this work.
                               -- However... why? 'eq?' works fine by just
                               -- comparing their StableNames and anything we
                               -- might want to test can be tested with 'eq?'.
eqVal (Closure p v b _ n) (Closure p' v' b' _ n') =
    and [ p == p'
        , v == v'
        , b == b'
        , n == n'
        ]
eqVal Nil Nil = True
eqVal p@IPair{} q@IPair{} =
    fromList p == fromList q
  where
    fromList :: Val -> ([Val], Val)
    fromList (IPair c d) =
      first (c:) $ fromList d
    fromList obj = ([], obj)

    first f (a, b) = (f a, b)
eqVal _ _ = False

-- Eq Val is defined here so that 'LispErr' can derive 'Eq'.
-- But it might be more sane to just not allow pure comparisons of
-- 'Val's or 'LispErr's. I think (?) it's only useful in tests
-- (and I'm not even sure that the tests use it anymore).

-- | Compares closures by shape rather than only by name/location tag.
-- Can't compare continuations (because they currently aren't tagged).
-- Can't compare mutable pairs (use 'valsSameShape' instead).
instance Eq Val where (==) = eqVal

-- | Only so that LispErr can derive Eq.
newtype ExceptionCont = EC (Val -> EM Val)
data LispErr = NumArgs Arity [Val]
             | TypeMismatch String Val
             | Parser String
             | BadSpecialForm Val
             | NotFunction String Val
             | UnboundVar String Symbol
             | EvaluateDuringInit Symbol
             | SetImmutable String -- type name
             | CircularList
             | EmptyBody

             | Condition (Maybe ExceptionCont) -- ^ continuable?
                         Val

             | Default String
             | Quit

-- This is not derived _purely_ so that we can compare
-- symbols by symbolName in two cases. Annoying.
instance Eq LispErr where
  NumArgs a1 vs1        == NumArgs a2 vs2        = a1 == a2 && vs1 == vs2
  TypeMismatch s1 v1    == TypeMismatch s2 v2    = s1 == s2 && v1 == v2
  Parser s1             == Parser s2             = s1 == s2
  BadSpecialForm v1     == BadSpecialForm v2     = v1 == v2
  NotFunction s1 v1     == NotFunction s2 v2     = s1 == s2 && v1 == v2
  UnboundVar s1 n1      == UnboundVar s2 n2      = s1 == s2 && symbolName n1 == symbolName n2
  EvaluateDuringInit n1 == EvaluateDuringInit n2 = symbolName n1 == symbolName n2
  SetImmutable s1       == SetImmutable s2       = s1 == s2
  CircularList          == CircularList          = True
  EmptyBody             == EmptyBody             = True
  Condition _ v1        == Condition _ v2        = v1 == v2
  Default s1            == Default s2            = s1 == s2
  Quit == Quit = True
  _ == _ = False

instance Eq ExceptionCont where
  EC _ == EC _ = True -- let the comparison be by everything else

type ThrowsError = Either LispErr

--trapError :: ThrowsError String -> ThrowsError String
--trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue Left{} = error "extractValue: Left"

isTerminationError :: LispErr -> Bool
isTerminationError Quit = True
isTerminationError _    = False

type IOThrowsError = ExceptT LispErr IO

--runIOThrows :: IOThrowsError String -> IO String
--runIOThrows action = extractValue . trapError <$> runExceptT action



{- Note: [EM return types]
Ideally, we'd like EM to be able to return any type, instead of just 'Val'.
MTL's Cont type handles this with a type variable; 'Cont r a'.

I tried lots of variations on EM that didn't require putting an 'r' type
everywhere. I couldn't make it work. The most promising attempt was
type EMCont r = EvalState -> IO (Either LispErr r)
newtype EMR r a = EM { unEM :: Cont (EMCont r) a }
  deriving...
type EM a = forall r. EMR r a

This almost works. In fact, if we didn't have continuations, it _would_
work. Alas, the need to be able to reify an EM computation and store it
into a Val required either making Val quantify over r (non-universally)
or making Continuation existential. The first doesn't solve the problem,
it just pushes the problem down onto Val. The second doesn't work because
we can't apply an existentially quantified continuation in 'apply', because
the 'r' in the type of 'apply' is universally quantified.

So instead, we force EM actions to produce a Val. This is OK, because we
can use a hack with IORefs to get the actual value we care about instead,
if needed.
-}

{- Note: [EM structure]
EM uses a little trick about continuation monads to embed state and errors,
instead of stacking StateT and ErrorT. The reason for this is that
ConT r (ErrorT e m) a doesn't (and in fact, can't!) provide a MonadError
instance. However, this type can.

Cont r a ~ (a -> r) -> r, so
Cont EMCont a ~ (a -> EvalState -> IO (...)) -> EvalState -> IO (...)
-}
type EMCont = EvalState -> IO (Either LispErr Val, EvalState)
-- | The Evaluation Monad
newtype EM a = EM { unEM :: Cont EMCont a }
  deriving (Functor, Applicative, Monad)

emCont :: ((a -> EMCont) -> EMCont) -> EM a
emCont = EM . cont

emThrow :: LispErr -> EM a
emThrow e = do
  handlersRef <- gets currentHandlers
  handlers    <- liftIO $ readIORef handlersRef
  handler     <- case handlers of
    [] -> error "emThrow: no handlers"
    [handler] -> pure handler -- leave initial handler alone
    (handler:cdrHandlers) ->
      liftIO $ writeIORef handlersRef cdrHandlers >> return handler
  _ <- handler e
  -- If the handler returns, we are to raise an exception with an unspecified
  -- relationship to e. For now, we simply re-raise e itself.
  -- This is moderate future-proofing. If the 'Error' objects ever became
  -- a primitive record type instead of a constructor of 'Val', we'd be in
  -- trouble here.
  emThrow e

-- | State is recovered from the point that 'emCatch' was invoked.
-- This function is for the MonadError EM instance and should not
-- be used in practice, as it doesn't know anything about
-- exception handlers.
emCatch :: EM a -> (LispErr -> EM a) -> EM a
emCatch (EM m) restore = emCont $ \k s -> do
    -- this leaves the catch frame around until the entire continuation
    -- chain, including the continuation of the catch itself, is done.
    -- That seems wrong, so perhaps this should be:
    {- mr <- runEM m (\x s -> pure (Right x, s)) a s
       case mr of
         (Left e, _s0) -> runEM (restore e) k a s
         (Right v, s') -> runEM (return v)  k a s'
    -}
    -- Edit, that doesn't work for type reasons but can work with anIORef.
    -- But why bother? We don't use this function.
    mr <- runCont m k s
    case mr of
        (Left e, _s0)  -> runCont (unEM (restore e)) k s
        (Right _v, _s) -> pure mr

emGet :: EM EvalState
emGet = emCont $ \k s -> k s s

emPut :: EvalState -> EM ()
emPut s = emCont $ \k _s -> k () s

emLiftIO :: IO a -> EM a
emLiftIO io = emCont $ \k s -> io >>= flip k s

-- | Invoking a continuation restores the state to when it was captured.
instance MonadCont EM where
  callCC f = emCont $ \k s ->
    runCont (unEM $ f (\x -> emCont $ \ _ _ -> k x s)) k s
  --{-# INLINE callCC #-}

instance MonadState EvalState EM where
  get = emGet
  --{-# INLINE get #-}
  put = emPut
  --{-# INLINE put #-}

-- | This instance made sense historically, but do the inability of the type
-- of 'catchError' to be instantiated by a function which uses 'Val'-specific
-- exception handlers, it no longer makes sense.
--
-- Consider it deprecated. However, when it is removed, 'EvaluationMonad' will
-- export a function named 'throwError' which behaves as a Haskell-level
-- version of raise.
instance MonadError LispErr EM where
  throwError = emThrow
  catchError = emCatch

instance MonadIO EM where
  liftIO = emLiftIO
  --{-# INLINE liftIO #-}

instance MonadFail EM where
  fail s = throwError . Default $
    "The following error occurred, please report a bug.\n" ++ s
  --{-# INLINE fail #-}

instance HasOpts EM where
  getOpts = gets options
  setOpts opts = modify $ \s -> s { options = opts }

liftIOThrows :: IOThrowsError a -> EM a
liftIOThrows = liftEither <=< liftIO . runExceptT

-- | The current state of evaluation
data EvalState = ES { globalEnv  :: GlobalEnv
                    , localEnv   :: LocalEnv
                    , stack      :: [StackFrame]
                    , dynEnv     :: DynamicEnv
                    , options    :: Opts
                    }

type ExceptionHandler = LispErr -> EM Val

-- | Haskell-level model of the dynamic Scheme environment.
-- Be aware that this is not a dynamic-extent? object
-- ala SRFI-154. It is the single representation of whichever
-- dynamic extent happens to be current at this point in Thcheme's
-- execution. It mutates as Thcheme executes.
data DynamicEnv = DynamicEnv
  { deDynPoint :: Ref DynamicPoint
  , deHandlers :: Ref [ExceptionHandler]
  }

dynPoint :: EvalState -> Ref DynamicPoint
dynPoint = deDynPoint . dynEnv

currentHandlers :: EvalState -> Ref [ExceptionHandler]
currentHandlers = deHandlers . dynEnv

-- | Associate a call in the callTrace with its captured local environment.
data StackFrame = StackFrame Val (Maybe LocalEnv)

-- | Dynamic points; this is the core of the implementation of dynamic-wind.
-- The details of the implementation are mainly sourced from
-- https://www.cs.hmc.edu/~fleck/envision/scheme48/meeting/node7.html
--
-- Each dynamic point stores the "before" and "after" thunks from the
-- dynamic-wind call, as well as the "parent" node, which is either the
-- root, or the dynamic point of the next innermost dynamic-wind call.
-- The root is the Point whose parent is the Sentinel, and its
-- before/after ref contains unspecified values that should not be called.
--
-- The implementation of dynamic-wind spans a couple of modules.
-- Here, continuation objects are defined as holding a DynamicPoint as
-- well as a function; this is the dynamic point of the dynamic extent in
-- which that continuation was captured. When a continuation is captured
-- by callWithCurrentContinuation in Misc.hs, it captures the current value of
-- dereferencing dynPoint as well. When a continuation is applied by
-- 'call' in Evaluation.hs, dynPoint is rerooted to the captured DynamicPoint
-- before the continuation's function itself is called. The implementation of
-- dynamicWind in Misc.hs contains two more rerootDynPoint calls that
-- cause the normal enter-and-exit calls to before and after.
data DynamicPoint = Sentinel | Point (Ref (Val, Val)) (Ref DynamicPoint)
  deriving Eq
