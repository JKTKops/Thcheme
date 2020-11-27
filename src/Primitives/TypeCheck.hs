{-# LANGUAGE LambdaCase #-}
module Primitives.TypeCheck (rawPrimitives) where

import Control.Monad.Except (throwError)

import Types

rawPrimitives = [ (name, guardOneArg func) | (name, func) <-
                    [ ("boolean?", bool)
                    , ("char?", char)
                    , ("list?", list)
                    , ("number?", number)
                    , ("pair?", pair)
                    , ("string?", string)
                    , ("symbol?", symbol)
                    , ("vector?", vector)
                    , ("procedure?", procedure)
                    , ("continuation?", continuation)
                    ]
                ]

guardOneArg :: (LispVal -> LispVal) -> RawPrimitive
guardOneArg func = RPrim 1 $ \case
    [x] -> return $ func x
    badArgs -> throwError $ NumArgs 1 badArgs

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
pair _ = Bool False

vector :: LispVal -> LispVal
vector (Vector _) = Bool True
vector _          = Bool False

procedure :: LispVal -> LispVal
procedure Primitive{}    = Bool True
procedure Continuation{} = Bool True
procedure Func{}         = Bool True
procedure _ = Bool False

continuation :: LispVal -> LispVal
continuation Continuation{} = Bool True
continuation _ = Bool False