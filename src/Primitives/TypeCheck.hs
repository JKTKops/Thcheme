{-# LANGUAGE LambdaCase #-}
module Primitives.TypeCheck (primitives) where

import Control.Monad.Except (throwError)

import LispVal

primitives :: [Primitive]
primitives = [ typePred name func 
             | (name, func) <-
               [ ("boolean", bool)
               , ("char", char)
               , ("number", number)
               , ("pair", pair)
               , ("string", string)
               , ("symbol", symbol)
               , ("vector", vector)
               , ("procedure", procedure)
               , ("continuation", continuation)
               ]
             ]

typePred :: String -> (LispVal -> LispVal) -> Primitive
typePred tyname func = Prim (tyname ++ "?") 1 $ \case
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

pair :: LispVal -> LispVal
pair (IList []) = Bool False
pair (IList _)  = Bool True
pair IDottedList{} = Bool True
pair Pair{} = Bool True
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