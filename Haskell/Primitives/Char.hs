module Primitives.Char (primitives) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C 
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import LispVal

primitives = [ (name, makeCharPrim Bool f) | (name, f) <- 
                [ ("char-alphabetic?", C.isAlpha)
                , ("char-numeric?", C.isNumber)
                , ("char-whitespace?", C.isSpace)
                , ("char-upper-case?", C.isUpper)
                , ("char-lower-case?", C.isLower)
                ] 
             ] ++
             [ (name, makeCharPrim Char f) | (name, f) <-
                [ ("char-upcase", C.toUpper)
                , ("char-downcase", C.toLower)
                ]
             ]

makeCharPrim :: (a -> LispVal) -> (Char -> a) -> RawPrimitive
makeCharPrim cons func [(Char c)] = return . cons $ func c
makeCharPrim _ _ [badArg]         = throwError $ TypeMismatch "char" badArg
makeCharPrim _ _ badArgs          = throwError $ NumArgs 1 badArgs