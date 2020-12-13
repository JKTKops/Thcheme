{-# LANGUAGE LambdaCase #-}
module Primitives.TypeCheck (primitives) where

import Control.Monad.Except (throwError)

import Val

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

typePred :: String -> (Val -> Val) -> Primitive
typePred tyname func = Prim (tyname ++ "?") 1 $ \case
  [x] -> return $ func x
  badArgs -> throwError $ NumArgs 1 badArgs

symbol :: Val -> Val
symbol (Atom _) = Bool True
symbol _        = Bool False

string :: Val -> Val
string (String _) = Bool True
string _          = Bool False

char :: Val -> Val
char (Char _) = Bool True
char _        = Bool False

number :: Val -> Val
number (Number _) = Bool True
number _          = Bool False

bool :: Val -> Val
bool (Bool _) = Bool True
bool _        = Bool False

pair :: Val -> Val
pair Pair{}  = Bool True
pair IPair{} = Bool True
pair _ = Bool False

vector :: Val -> Val
vector (Vector _) = Bool True
vector _          = Bool False

procedure :: Val -> Val
procedure Primitive{}    = Bool True
procedure Continuation{} = Bool True
procedure Closure{}      = Bool True
procedure _ = Bool False

continuation :: Val -> Val
continuation Continuation{} = Bool True
continuation _ = Bool False