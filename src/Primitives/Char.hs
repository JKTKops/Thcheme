{-# LANGUAGE LambdaCase #-}
module Primitives.Char (rawPrimitives) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import Types

rawPrimitives = [ (name, makeCharPrim Bool f) | (name, f) <-
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
makeCharPrim cons func = RPrim 1 $ \case
    [Char c] -> return . cons $ func c
    [badArg] -> throwError $ TypeMismatch "char" badArg
    badArgs  -> throwError $ NumArgs 1 badArgs
