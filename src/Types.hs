{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
             | Func { params  :: [String]
                    , vararg  :: Maybe String
                    , body    :: [LispVal]
                    , closure :: Env
                    , name    :: Maybe String
                    }
             | PMacro Arity Builtin String
             | Port Handle

instance Eq LispVal where (==) = eqVal

instance Show LispVal where show = showVal

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String String
             | UnboundVar String String
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
eqVal (Func p v b _ n) (Func p' v' b' _ n') = and [ p == p'
                                                  , v == v'
                                                  , b == b'
                                                  , n == n'
                                                  ]
eqVal _ _ = False

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (Number n) = show n
showVal (String s) = show s
showVal (Char c)   = "#\\" ++ case c of
    ' '  -> "space"
    '\t' -> "tab"
    '\n' -> "newline"
    '\r' -> "carriage-return"
    _    -> pure c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordsList ls ++ ")"
showVal (DottedList ls l) = "(" ++ unwordsList ls ++ " . " ++ show l ++ ")"
showVal (Vector v) = "#(" ++ unwordsList (elems v) ++ ")"
showVal (Port _) = "<Port>"
showVal (Primitive _ _ name) = "<Function " ++ name ++ ">"
showVal (Func args varargs body env name) = "(" ++ fromMaybe "lambda" name
    ++ " (" ++ unwords args ++ (case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (PMacro _ _ name) = "<Macro " ++ name ++ ">"

showErr :: LispErr -> String
showErr (UnboundVar message varname)  = message ++ ": " ++ varname
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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- | The Evaluation Monad
type EMt = ExceptT LispErr (StateT EvalState IO)
newtype EM a = EM { runEM :: EMt a }
  deriving ( Monad, Functor, Applicative, MonadIO
           , MonadError LispErr, MonadState EvalState)

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
        numberedLines = unlines $ zipWith (\n e -> n ++ " " ++ e) numbers exprs
        numbers = map (\i -> show i ++ ";") [1..]

        fehOpt :: TraceType
        fehOpt =
            case truthy <$> Map.lookup "full-evaluation-history" (options es) of
                Just True -> FullHistory
                _         -> CallOnly

        exprs = if fehOpt == CallOnly
                then map (show . snd) . filter ((== Call) . fst) $ stack es
                else map (\(s, v) ->
                         let buffer = case s of
                                 Call -> "    "
                                 Reduce -> "  "
                                 Expand -> "  "
                         in show s ++ ":" ++ buffer ++ show v)
                     $ stack es

        truthy :: LispVal -> Bool
        truthy v = not $ v == Bool False
                      || v == Number 0
                      || v == List []
                      || v == String ""
