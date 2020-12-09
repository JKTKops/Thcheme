{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types
    ( Env
    , Arity
    , RBuiltin
    , IBuiltin
    , Builtin
    , RawPrimitive (..)
    , IOPrimitive (..)
    , Primitive (..)
    , Macro (..)
    , Val (..), PairObj (..)
    , Ref
    , truthy
    , LispErr (..)
    , ThrowsError
    , IOThrowsError
    , trapError
    , extractValue
    , isTerminationError
    , liftEither
    , runIOThrows
    , EM (..)
    , EvalState (..)
    , StepReason (..)
    , Opts
    , TraceType
    , liftIOThrows
    ) where

import Text.ParserCombinators.Parsec (ParseError)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Array
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.State.Lazy
import System.IO (Handle)

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
type Ref = IORef

-- * Function types and components
type Arity = Integer
type RBuiltin = [Val] -> ThrowsError Val
data RawPrimitive = RPrim Arity RBuiltin
type IBuiltin = [Val] -> IOThrowsError Val
data IOPrimitive = IPrim Arity IBuiltin
type Builtin = [Val] -> EM Val
data Primitive = Prim String Arity Builtin
data Macro = Macro Arity Builtin


-- TODO maybe R5RS numeric tower, or just some sort of float at least
-- | Scheme values.
--
-- Some Scheme values can be either mutable or immutable. These are strings,
-- lists, and vectors. The immutable variant begins with I, where applicable.
-- The mutable variant of both IList and IDottedList is Pair.
--
-- Invariant: immutable lists are finite.
data Val
  = Atom String
  | IList [Val]
  | IDottedList [Val] Val
  | Vector (Array Integer Val)
  | Number Integer
  | String String
  | Char Char
  | Bool Bool
  | Primitive Arity Builtin String
  | PrimMacro Arity Builtin String
  | Continuation EvalState (Val -> EM Val)
  | Func { params  :: [String]
         , vararg  :: Maybe String
         , body    :: [Val]
         , closure :: Env
         , name    :: Maybe String
         }
  | Port Handle
  | Undefined
  | PairPtr (IORef PairObj)

data PairObj = PairObj !(IORef Val) !(IORef Val)

-- TODO: [r7rs]
-- Defined here because it's used in `showEs` 12/6/2020.
-- If stack changes in the future (perhaps to be properly tail-recursive)
-- and this can be moved to Val.hs, do so.
-- | Is this 'Val' truthy?
truthy :: Val -> Bool
truthy (Bool False) = False
truthy _ = True

instance Eq Val where (==) = eqVal

instance Show Val where
    showsPrec _ v s = showVal v s

data LispErr = NumArgs Integer [Val]
             | TypeMismatch String Val
             | Parser ParseError
             | BadSpecialForm String Val
             | NotFunction String Val
             | UnboundVar String String
             | EvaluateDuringInit String 
             | SetImmutable String -- type name
             | CircularList
             | Default String
             | Quit
    deriving (Eq)

instance Show LispErr where show = ("Error: " ++ ) . showErr

type ThrowsError = Either LispErr

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

isTerminationError :: ThrowsError Val -> Bool
isTerminationError (Left Quit) = True
isTerminationError _           = False

type IOThrowsError = ExceptT LispErr IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue . trapError <$> runExceptT action

eqVal :: Val -> Val -> Bool
eqVal (Atom s) (Atom s') = s == s'
eqVal (Number n) (Number n') = n == n'
eqVal (String s) (String s') = s == s'
eqVal (Char c) (Char c') = c == c'
eqVal (Bool b) (Bool b') = b == b'
eqVal (IList ls) (IList ls') = ls == ls'
eqVal (IDottedList ls l) (IDottedList ls' l') = ls == ls' && l == l'
eqVal (Vector v) (Vector v') = v == v'
eqVal (Port p) (Port p') = p == p'
eqVal (Primitive _ _ n) (Primitive _ _ n') = n == n'
eqVal Continuation{} _ = False -- perhaps it's possible to give continuations
eqVal _ Continuation{} = False -- unique identifiers to make this work.
                               -- The state would have to have an IORef holding
                               -- the counter to preserve it between repl
                               -- steps / continuation invocations.
eqVal (Func p v b _ n) (Func p' v' b' _ n') = and [ p == p'
                                                  , v == v'
                                                  , b == b'
                                                  , n == n'
                                                  ]
eqVal _ _ = False

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
showVal (IList ls) = showParen True $ unwordsList ls
showVal (IDottedList ls l) = showParen True $
    unwordsList ls . showString " . " . shows l
showVal (Vector v) = showChar '#' . showParen True (unwordsList (elems v))
showVal Port{} = showString "#<port>"
showVal Undefined = showString "#<undefined>"
showVal (Primitive _ _ name) = showString $ "#<function " ++ name ++ ">"
showVal Continuation{} = showString "#<cont>"
showVal (Func args varargs body env name) = 
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
showVal PairPtr{} = showString "<can't show mutable pair>"

showErr :: LispErr -> String
showErr (UnboundVar message varname)  = message ++ ": " ++ varname
showErr (EvaluateDuringInit name) = name ++ " referred to itself during initialization"
showErr (SetImmutable tyname) = "can't set immutable " ++ tyname
showErr (BadSpecialForm message form) = message ++ ": " ++ show form
showErr (NotFunction message func)    = message ++ ": " ++ show func
showErr (NumArgs expected found)      = "expected " ++ show expected
    ++ " arg" ++ (if expected == 1
        then ""
        else "s")
    ++ "; found values " ++ show found
showErr (TypeMismatch expected found) = "invalid type: expected " ++ expected
    ++ ", found " ++ show found
showErr CircularList                  = "circular list"
showErr (Parser parseErr)             = "parse error at " ++ show parseErr
showErr (Default message)             = message
showErr Quit                          = "quit invoked"

-- I just snagged this from a different project
intercalateS :: String -> [ShowS] -> ShowS
intercalateS sep = go
  where go []     = id
        go [s]    = s
        go (s:ss) = s . showString sep . go ss

unwordsList :: [Val] -> ShowS
unwordsList = intercalateS " " . map shows

-- ideally; EMCont = forall r. EvalState -> IO (Either LispErr r)
-- but GHC screams at that and also I'm not sure how to recover the EvalState
-- to implement evalEM properly so that a partial state is returned even if an
-- error occurs.
-- So instead we have to force the type that evalEM is allowed to produce.
-- Note that complexity on the right-hand side of the arrow doesn't affect
-- the amount of work performed at any point because it is merely the eventual
-- return of a continuation.
type EMCont = EvalState -> IO (Either LispErr Val, EvalState)
-- | The Evaluation Monad
newtype EM a = EM { unEM :: Cont EMCont a }
  deriving (Functor, Applicative, Monad, MonadCont)

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

instance MonadState EvalState EM where
    get = emGet
    put = emPut

instance MonadError LispErr EM where
    throwError = emThrow
    catchError = emCatch

instance MonadIO EM where
    liftIO = emLiftIO

instance MonadFail EM where
    fail s = throwError . Default $ s ++ "\nAn error occurred, please report a bug."

liftIOThrows :: IOThrowsError a -> EM a
liftIOThrows = liftEither <=< liftIO . runExceptT

-- | Reasons a step was performed
data StepReason = Call | Reduce | Expand deriving (Eq, Show, Read, Enum)
-- | Type of options from the REPL
type Opts = Map.HashMap String Val

-- | The current state of evaluation
data EvalState = ES { stack      :: [(StepReason, Val)]
                    , symEnv     :: [Env]
                    , options    :: Opts
                    }

instance Show EvalState where show = showEs

data TraceType = CallOnly | FullHistory deriving (Eq, Show, Read, Enum)
showEs :: EvalState -> String
showEs es = "Stack trace:\n" ++ numberedLines
  where numberedLines :: String
        numberedLines = unlines $ zipWith (<+>) numbers exprs
        numbers = map (\i -> show i ++ ";") [1..]

        fehOpt :: TraceType
        fehOpt =
            case truthy <$> Map.lookup "full-stack-trace" (options es) of
                Just True -> FullHistory
                _         -> CallOnly

        exprs = if fehOpt == CallOnly
                then map (show . snd) . filter ((`elem` [Call, Expand]) . fst) $ stack es
                else map (\(s, v) ->
                         let buffer = case s of
                                 Call -> "    "
                                 Reduce -> "  "
                                 Expand -> "  "
                         in show s ++ ":" ++ buffer ++ show v)
                     $ stack es

(<+>) :: String -> String -> String
"" <+> s  = s
s  <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2
