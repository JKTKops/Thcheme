{-# LANGUAGE LambdaCase #-}
module Primitives.Char (primitives) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import Val

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

makeCharPrim :: String -> (a -> Val) -> (Char -> a) -> Primitive
makeCharPrim name constr func = Prim name 1 $ \case
    [Char c] -> return . constr $ func c
    [badArg] -> throwError $ TypeMismatch "char" badArg
    badArgs  -> throwError $ NumArgs 1 badArgs
