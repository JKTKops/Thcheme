module LispVal (
    Env
    , LispVal (..)
    , LispErr (..)
    , ThrowsError
    , trapError
    , extractValue
    ) where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Except (throwError, catchError)
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

-- TODO maybe R5RS numeric tower, or just some sort of float at least
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Char Char
             | Bool Bool
             | Primitive ([LispVal] -> ThrowsError LispVal) String
             | Func { params  :: [String]
                    , vararg  :: Maybe String
                    , body    :: [LispVal]
                    , closure :: Env 
                    , name    :: Maybe String
                    }

instance Show LispVal where show = showVal

data LispErr = NumArgs Integer [LispVal]
             | TypeMismatch String LispVal
             | Parser ParseError
             | BadSpecialForm String LispVal
             | NotFunction String String
             | UnboundVar String String
             | Default String

instance Show LispErr where show = ("Error: " ++ ) . showErr

type ThrowsError = Either LispErr

trapError :: ThrowsError String -> ThrowsError String 
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showVal :: LispVal -> String
showVal (Atom s) = s
showVal (Number n) = show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Char c)   = "#\\" ++ case c of
    ' '       -> "space"
    '\t'      -> "tab"
    '\n'      -> "newline"
    otherwise -> pure c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List ls) = "(" ++ unwordsList ls ++ ")"
showVal (DottedList ls l) = "(" ++ unwordsList ls ++ " . " ++ show l ++ ")"
showVal (Primitive _ name) = "<Function " ++ name ++ ">"
showVal (Func args varargs body env name) = "(" ++ (maybe "lambda" id name)
    ++ " (" ++ unwords args ++ (case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
