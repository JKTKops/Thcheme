{-# LANGUAGE LambdaCase #-}
module Primitives.Char (primitives) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import Val
import EvaluationMonad (panic)

primitives :: [Primitive]
primitives = [ makeCharPrim name Bool f 
             | (name, f) <-
               [ ("char-alphabetic?", C.isAlpha)
               , ("char-numeric?", C.isNumber)
               , ("char-whitespace?", C.isSpace)
               , ("char-upper-case?", C.isUpper)
               , ("char-lower-case?", C.isLower)
               ]
              ] ++
              [ makeCharPrim name Char f
              | (name, f) <-
                [ ("char-upcase", C.toUpper)
                , ("char-downcase", C.toLower)
                ]
              ]

makeCharPrim :: Symbol -> (a -> Val) -> (Char -> a) -> Primitive
makeCharPrim name constr func = Prim name (Exactly 1) $ \case
  [Char c] -> return . constr $ func c
  [badArg] -> throwError $ TypeMismatch "char" badArg
  _ -> panic $ "makeCharPrim@" ++ symbolAsString name ++ " arity"
