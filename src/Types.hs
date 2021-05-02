{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types
    ( Env, GlobalEnv, LocalEnv
    , Arity (..)
    , InTail
    , Builtin
    , Primitive (..)
    , Macro (..)
    , Val (..), RealNumber (..), Number (..)
    , Ref
    , LispErr (..)
    , ThrowsError
    , IOThrowsError
    , extractValue
    , isTerminationError
    , liftEither
   -- , runIOThrows
    , EM (..)
    , EvalState (..)
    , StackFrame (..)
    , Opts
   -- , TraceType
    , liftIOThrows
    ) where

import Control.Monad.Cont ( (<=<), Cont, ContT(ContT), MonadCont(..)
                          , MonadIO(..), cont, runCont )
import Control.Monad.Except           ( ExceptT, MonadError(..), liftEither
                                      , runExceptT )
import Control.Monad.Fail             ( MonadFail(..) )
import Control.Monad.State.Lazy       ( MonadState(get, put), gets, modify )
import Data.Complex                   ( Complex(..) )
import Data.HashMap.Strict            ( HashMap )
import Data.IORef                     ( IORef )
import Data.Ratio                     ( (%), denominator, numerator )
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
type Env = IORef (HashMap String (IORef Val))
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
data Primitive = Prim String Arity Builtin
-- The additional 'InTail' flag is necessary for macros like 'begin'
-- and 'if' which need to use a different 'eval' behavior if they are
-- in tail position. We do global CPS anyway (EM monad) but this information
-- is needed to maintain the debug stack.
data Macro = Macro Arity (InTail -> Builtin)


-- TODO maybe R5RS numeric tower, or just some sort of float at least
-- | Scheme values.
--
-- Some Scheme values can be either mutable or immutable. These are strings,
-- lists, and vectors. The immutable variant begins with I, where applicable.
--
-- Invariant: All objects contained in an IPair or IVector are immutable.
--   Note that this is implicitly transitive.
data Val
  = Symbol String

  | Number !Number

  | String !(Ref String)
  | IString String
  | Char Char
  | Bool Bool

  | Primitive Arity Builtin String
  | PrimMacro Arity (InTail -> Builtin) String
  | Continuation (Val -> EM Val)
  | Closure
     { params :: [String]
     , vararg :: Maybe String
     , body   :: [Val]
     , cloEnv :: LocalEnv
     , name   :: Maybe String
     }
  | MacroTransformer String (Val -> EM Val)
    
  | Port Handle
  | Undefined

  | Nil
  | Pair !(Ref Val) !(Ref Val)
  | IPair Val Val

  | Vector  !(V.IOVector Val)
  | IVector !(V.Vector Val)
  | IByteVector !(U.Vector Word8)

-- | Scheme numbers. Note that there is no Ord Number instance;
-- complex numbers are not orderable and Scheme comparisons of
-- complex numbers always return #f.
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

data LispErr = NumArgs Arity [Val]
             | TypeMismatch String Val
             | Parser String
             | BadSpecialForm Val
             | NotFunction String Val
             | UnboundVar String String
             | EvaluateDuringInit String 
             | SetImmutable String -- type name
             | CircularList
             | EmptyBody
             | Default String
             | Quit
    deriving (Eq)

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
EM a ~ (a -> EvalState -> IO (...)) -> EvalState -> IO (...).
-}

type EMCont = EvalState -> IO (Either LispErr Val, EvalState)
-- | The Evaluation Monad
newtype EM a = EM { unEM :: Cont EMCont a }
  deriving (Functor, Applicative, Monad)

emCont :: ((a -> EMCont) -> EMCont) -> EM a
emCont = EM . cont

emThrow :: LispErr -> EM a
emThrow e = emCont $ \_k s -> pure (Left e, s)

-- | State is recovered from the point that the error was thrown, rather
-- than from the point 'emCatch' is invoked.
emCatch :: EM a -> (LispErr -> EM a) -> EM a
emCatch (EM m) restore = emCont $ \k s -> do
    mr <- runCont m k s
    case mr of
        (Left e, s0)   -> runCont (unEM (restore e)) k s0
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
        runCont (unEM (f (\x -> emCont $ \_ _ -> k x s))) k s

instance MonadState EvalState EM where
    get = emGet
    put = emPut

instance MonadError LispErr EM where
    throwError = emThrow
    catchError = emCatch

instance MonadIO EM where
    liftIO = emLiftIO

instance MonadFail EM where
    fail s = throwError . Default $ 
        "The following error occurred, please report a bug.\n" ++ s

instance HasOpts EM where
    getOpts = gets options
    setOpts opts = modify $ \s -> s { options = opts }

liftIOThrows :: IOThrowsError a -> EM a
liftIOThrows = liftEither <=< liftIO . runExceptT

-- | The current state of evaluation
data EvalState = ES { globalEnv  :: GlobalEnv
                    , localEnv   :: LocalEnv
                    , stack      :: [StackFrame]
                    , options    :: Opts
                    }

-- | Associate a call in the callTrace with its captured local environment.
data StackFrame = StackFrame Val (Maybe LocalEnv)
