module Primitives.TypeTransformers 
    ( charToNumber
    , numberToString
    , numberToChar
    , stringToNumber
    ) where
    
import Data.Char (chr, ord)
import Control.Monad.Except (throwError)

import LispVal

charToNumber    = typeTransformer charToNumberH
numberToString  = typeTransformer numberToStringH
numberToChar    = typeTransformer numberToCharH
stringToNumber  = typeTransformer stringToNumberH

typeTransformer :: (LispVal -> ThrowsError LispVal) -- transformer
                -> [LispVal]
                -> ThrowsError LispVal
typeTransformer t [x]     = t x
typeTransformer _ badArgs = throwError $ NumArgs 1 badArgs

numberToStringH :: LispVal -> ThrowsError LispVal
numberToStringH (Number n) = return . String $ show n
numberToStringH notNum     = throwError $ TypeMismatch "number" notNum

stringToNumberH :: LispVal -> ThrowsError LispVal
stringToNumberH (String s) = let parsed = reads s in
    if null parsed
    then return $ Bool False
    else return . Number . fst $ parsed !! 0
stringToNumberH notStr     = throwError $ TypeMismatch "string" notStr

numberToCharH :: LispVal -> ThrowsError LispVal
numberToCharH (Number n) = return . Char . chr $ fromIntegral n
numberToCharH notNum     = throwError $ TypeMismatch "number" notNum

charToNumberH :: LispVal -> ThrowsError LispVal
charToNumberH (Char c) = return . Number . fromIntegral $ ord c
charToNumberH notChar  = throwError $ TypeMismatch "char" notChar
