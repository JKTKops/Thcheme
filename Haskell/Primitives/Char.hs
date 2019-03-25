module Primitives.Char (primitives) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C 
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import LispVal

primitives = (mapSnd (makeCharPrim Bool) 
                [ ("char-alphabetic?", C.isAlpha)
                , ("char-numeric?", C.isNumber)
                , ("char-whitespace?", C.isSpace)
                , ("char-upper-case?", C.isUpper)
                , ("char-lower-case?", C.isLower)
                ]) ++
             (mapSnd (makeCharPrim Char)
                [ ("char-upcase", C.toUpper)
                , ("char-downcase", C.toLower)
                ])
  where mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
        mapSnd _ [] = []
        mapSnd f ((x, y):ps) = (x, f y): mapSnd f ps
        
makeCharPrim :: (a -> LispVal) -> (Char -> a) -> RawPrimitive
makeCharPrim cons func [(Char c)] = return . cons $ func c
makeCharPrim _ _ [badArg]         = throwError $ TypeMismatch "char" badArg
makeCharPrim _ _ badArgs          = throwError $ NumArgs 1 badArgs
