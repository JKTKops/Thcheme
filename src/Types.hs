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
    , LispVal (..)
    , truthy
    , canonicalizeList
    , LispErr (..)
    , ThrowsError
    , IOThrowsError
    , trapError
    , extractValue
    , isTerminationError
    , liftThrows
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

type Env = IORef (HashMap String (IORef LispVal))

-- * Function types and components
type Arity = Integer
type RBuiltin = [LispVal] -> ThrowsError LispVal
data RawPrimitive = RPrim Arity RBuiltin
type IBuiltin = [LispVal] -> IOThrowsError LispVal
data IOPrimitive = IPrim Arity IBuiltin
type Builtin = [LispVal] -> EM LispVal
data Primitive = Prim Arity Builtin
data Macro = Macro Arity Builtin

-- TODO maybe R5RS numeric tower, or just some sort of float at least
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Integer LispVal)
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | Primitive Arity Builtin String
             | Continuation EvalState (LispVal -> EM LispVal)
             | Func { params  :: [String]
                    , vararg  :: Maybe String
                    , body    :: [LispVal]
                    , closure :: Env
                    , name    :: Maybe String
                    }
             | PMacro Arity Builtin String
             | Port Handle
               -- Undefined values are only created when they are stored
               -- immediately into an environment and an error is thrown
               -- immediately when they are looked up. Therefore, they should
               -- never appear, well, anywhere, really. Used for 'define'
               -- to create a location before evaluating the value.
             | Undefined

instance Eq LispVal where (==) = eqVal

instance Show LispVal where
    showsPrec _ v s = showVal v s

truthy :: LispVal -> Bool
truthy v = not $ v == Bool False
              || v == Number 0
              || v == List []
              || v == String ""

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String LispVal
             | UnboundVar String String
             | EvaluateDuringInit String
             | Default String
             | Quit
    deriving (Eq)

instance Show LispErr where show = ("Error: " ++ ) . showErr

type ThrowsError = Either LispErr

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

isTerminationError :: ThrowsError LispVal -> Bool
isTerminationError (Left Quit) = True
isTerminationError _           = False

type IOThrowsError = ExceptT LispErr IO

liftThrows :: ThrowsError a -> IOThrowsError a
           -- (MonadError m a) => Either e a -> m a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue . trapError <$> runExceptT action

eqVal :: LispVal -> LispVal -> Bool
eqVal (Atom s) (Atom s') = s == s'
eqVal (Number n) (Number n') = n == n'
eqVal (String s) (String s') = s == s'
eqVal (Char c) (Char c') = c == c'
eqVal (Bool b) (Bool b') = b == b'
eqVal (List ls) (List ls') = ls == ls'
eqVal (DottedList ls l) (DottedList ls' l') = ls == ls' && l == l'
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

showVal :: LispVal -> ShowS
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
showVal (List ls) = showParen True $ unwordsList ls
showVal (DottedList ls l) = showParen True $
    unwordsList ls . showString " . " . shows l
showVal (Vector v) = showChar '#' . showParen True (unwordsList (elems v))
showVal Port{} = showString "<port>"
showVal Undefined = showString "#<undef>"
showVal (Primitive _ _ name) = showString $ "#<function " ++ name ++ ">"
showVal Continuation{} = showString "<cont>"
showVal (Func args varargs body env name) = 
    showParen True $
        displayName . showChar ' '
      . showParen True (displayArgs . displayVarargs)
      . showString " ..."
        
{-        "(" ++ fromMaybe "lambda" name
    ++ " (" ++ unwords args ++ (case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"-}
  where
    displayName = showString $ fromMaybe "lambda" name
    displayArgs = showString $ unwords args
    displayVarargs = case varargs of
        Nothing  -> id
        Just arg -> showString " . " . showString arg
showVal (PMacro _ _ name) = showString $ "#<macro " ++ name ++ ">"

canonicalizeList :: LispVal -> LispVal
canonicalizeList (DottedList lst (List cdr)) = List (lst ++ cdr)
canonicalizeList other = other

showErr :: LispErr -> String
showErr (UnboundVar message varname)  = message ++ ": " ++ varname
showErr (EvaluateDuringInit name) = name ++ " referred to itself during initialization"
showErr (BadSpecialForm message form) = message ++ ": " ++ show form
showErr (NotFunction message func)    = message ++ ": " ++ show func
showErr (NumArgs expected found)      = "Expected " ++ show expected
    ++ " arg" ++ (if expected == 1
        then ""
        else "s")
    ++ "; found values " ++ show found
showErr (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showErr (Parser parseErr)             = "Parse error at " ++ show parseErr
showErr (Default message)             = message
showErr Quit                          = "quit invoked"

-- I just snagged this from a different project
intercalateS :: String -> [ShowS] -> ShowS
intercalateS sep = go
  where go []     = id
        go [s]    = s
        go (s:ss) = s . showString sep . go ss

unwordsList :: [LispVal] -> ShowS
unwordsList = intercalateS " " . map shows

-- ideally; EMCont = forall r. EvalState -> IO (Either LispErr r)
-- but GHC screams at that and also I'm not sure how to recover the EvalState
-- to implement evalEM properly so that a partial state is returned even if an
-- error occurs.
-- So instead we have to force the type that evalEM is allowed to produce.
-- Note that complexity on the right-hand side of the arrow doesn't affect
-- the amount of work performed at any point (except by the only higher-order
-- effect, emCatch) because it is merely the eventual return of a continuation.
type EMCont = EvalState -> IO (Either LispErr LispVal, EvalState)
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
type Opts = Map.HashMap String LispVal

-- | The current state of evaluation
data EvalState = ES { stack      :: [(StepReason, LispVal)]
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
