module Primitives.TypeCheck
    ( isSymbol
    , isString
    , isChar
    , isNumber
    , isBool
    , isList
    , isPair
    ) where
    
import Control.Monad.Except (throwError)

import LispVal

isSymbol = guardOneArg symbol
isString = guardOneArg string
isChar   = guardOneArg char
isNumber = guardOneArg number
isBool   = guardOneArg bool
isList   = guardOneArg list
isPair   = guardOneArg pair

guardOneArg :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
guardOneArg func [x] = return $ func x
guardOneArg _ args   = throwError $ NumArgs 1 args

symbol :: LispVal -> LispVal
symbol (Atom _) = Bool True
symbol _        = Bool False

string :: LispVal -> LispVal
string (String _) = Bool True
string _          = Bool False

char :: LispVal -> LispVal
char (Char _) = Bool True
char _        = Bool False

number :: LispVal -> LispVal
number (Number _) = Bool True
number _          = Bool False

bool :: LispVal -> LispVal
bool (Bool _) = Bool True
bool _        = Bool False

list :: LispVal -> LispVal
list (List _) = Bool True
list _        = Bool False

pair :: LispVal -> LispVal
pair (List []) = Bool False
pair (List _)  = Bool True
pair (DottedList _ _) = Bool True
