{-# LANGUAGE LambdaCase #-}
module Primitives.TypeCheck (primitives) where

import Val
import EvaluationMonad (panic)
import Primitives.String (stringSH)

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
typePred tyname func = Prim (tyname ++ "?") (Exactly 1) $ \case
  [x] -> return $ func x
  _ -> panic $ "typePred@" ++ tyname ++ " arity"

symbol :: Val -> Val
symbol (Symbol _) = Bool True
symbol _          = Bool False

string :: Val -> Val
string = Bool . stringSH

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
pair = Bool . pairSH

vector :: Val -> Val
vector = Bool . vectorSH

procedure :: Val -> Val
procedure Primitive{}    = Bool True
procedure Continuation{} = Bool True
procedure Closure{}      = Bool True
procedure _ = Bool False

continuation :: Val -> Val
continuation Continuation{} = Bool True
continuation _ = Bool False
