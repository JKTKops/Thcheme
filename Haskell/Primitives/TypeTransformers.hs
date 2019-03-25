module Primitives.TypeTransformers 
    ( charToNumber
    , charToString
    , listToString
    , numberToString
    , numberToChar
    , stringToList
    , stringToNumber
    ) where
    
import Data.Char (chr, ord)
import Control.Monad.Except (throwError)

import LispVal

charToNumber    = typeTransformer charToNumberH
charToString    = typeTransformer charToStringH
listToString    = typeTransformer listToStringH
numberToString  = typeTransformer numberToStringH
numberToChar    = typeTransformer numberToCharH
stringToList    = typeTransformer stringToListH
stringToNumber  = typeTransformer stringToNumberH

typeTransformer :: (LispVal -> ThrowsError LispVal) -- transformer
                -> [LispVal]
                -> ThrowsError LispVal
typeTransformer t [x]     = t x
typeTransformer _ badArgs = throwError $ NumArgs 1 badArgs

charToNumberH :: LispVal -> ThrowsError LispVal
charToNumberH (Char c) = return . Number . fromIntegral $ ord c
charToNumberH notChar  = throwError $ TypeMismatch "char" notChar

charToStringH :: LispVal -> ThrowsError LispVal
charToStringH (Char c) = return $ String [c]
charToStringH notChar  = throwError $ TypeMismatch "char" notChar

listToStringH :: LispVal -> ThrowsError LispVal
listToStringH (List chars) = return . String =<< mapchars chars where
    mapchars :: [LispVal] -> ThrowsError String
    mapchars []              = return $ []
    mapchars ((Char c) : cs) = return . (c :) =<< mapchars cs
    mapchars (notChar : _)   = throwError $ TypeMismatch "char" notChar
listToStringH notList      = throwError $ TypeMismatch "list" notList

numberToCharH :: LispVal -> ThrowsError LispVal
numberToCharH (Number n) = return . Char . chr $ fromIntegral n
numberToCharH notNum     = throwError $ TypeMismatch "number" notNum

numberToStringH :: LispVal -> ThrowsError LispVal
numberToStringH (Number n) = return . String $ show n
numberToStringH notNum     = throwError $ TypeMismatch "number" notNum

stringToListH :: LispVal -> ThrowsError LispVal
stringToListH (String s) = return . List $ map Char s
stringToListH notStr     = throwError $ TypeMismatch "string" notStr

stringToNumberH :: LispVal -> ThrowsError LispVal
stringToNumberH (String s) = let parsed = reads s in
    if null parsed
    then return $ Bool False
    else return . Number . fst $ parsed !! 0
stringToNumberH notStr     = throwError $ TypeMismatch "string" notStr
