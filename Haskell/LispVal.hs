module LispVal 
    ( Env
    , RawPrimitive
    , IOPrimitive
    , LispVal (..)
    , LispErr (..)
    , ThrowsError
    , IOThrowsError
    , trapError
    , extractValue
    , isTerminationError
    , liftThrows
    , runIOThrows
    ) where

import Text.ParserCombinators.Parsec (ParseError)
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Control.Monad.Except (liftM, liftIO, throwError, catchError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import System.IO (Handle)



-- Defined here to avoid circular dependencies
type Env = IORef (HashMap String (IORef LispVal))
type RawPrimitive = [LispVal] -> ThrowsError LispVal
type IOPrimitive  = [LispVal] -> IOThrowsError LispVal

-- TODO maybe R5RS numeric tower, or just some sort of float at least
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | Primitive RawPrimitive String
             | Func { params  :: [String]
                    , vararg  :: Maybe String
                    , body    :: [LispVal]
                    , closure :: Env 
                    , name    :: Maybe String
                    }
             | IOPrimitive IOPrimitive String
             | Port Handle

instance Show LispVal where show = showVal

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String String
             | UnboundVar String String
             | Default String
             | Quit

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
runIOThrows action = (extractValue . trapError) <$> runExceptT action

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (Number n) = show n
showVal (String s) = show s
showVal (Char c)   = "#\\" ++ case c of
    ' '       -> "space"
    '\t'      -> "tab"
    '\n'      -> "newline"
    '\r'      -> "carriage-return"
    _         -> pure c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordsList ls ++ ")"
showVal (DottedList ls l) = "(" ++ unwordsList ls ++ " . " ++ show l ++ ")"
showVal (Port _) = "<Port>"
showVal (Primitive _ name) = "<Function " ++ name ++ ">"
showVal (Func args varargs body env name) = "(" ++ fromMaybe "lambda" name
    ++ " (" ++ unwords args ++ (case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOPrimitive _ name) = "<Function " ++ name ++ ">"

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
