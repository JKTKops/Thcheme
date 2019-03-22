module Primitives.Char 
    ( toLower
    , toUpper
    , isAlpha
    , isNumber
    , isSpace
    , isUpper
    , isLower
    ) where

import Control.Monad.Except (throwError)
import qualified Data.Char as C 
    (toLower, toUpper, isAlpha, isNumber, isSpace, isUpper, isLower)

import LispVal

charTest :: (Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charTest func [(Char c)] = return . Bool $ func c
charTest _ badArgs       = throwError $ NumArgs 1 badArgs

isAlpha  = charTest C.isAlpha
isNumber = charTest C.isNumber
isSpace  = charTest C.isSpace
isUpper  = charTest C.isUpper
isLower  = charTest C.isLower

charTrans :: (Char -> Char) -> [LispVal] -> ThrowsError LispVal
charTrans func [(Char c)] = return . Char $ func c
charTrans _ badArgs      = throwError $ NumArgs 1 badArgs

toLower = charTrans C.toLower
toUpper = charTrans C.toUpper
