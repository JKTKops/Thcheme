module Primitives.TypeTransformers (primitives) where
    
import Data.Char (chr, ord)
import Control.Monad.Except (throwError)

import LispVal

primitives = [ (name, typeTransformer transform) | (name, transform) <-
                 [ ("char->number", charToNumber)
                 , ("char->string", charToString)
                 , ("list->string", listToString)
                 , ("number->string", numberToString)
                 , ("number->char", numberToChar)
                 , ("string->list", stringToList)
                 , ("string->number", stringToNumber)
                 ]
             ]

typeTransformer :: (LispVal -> ThrowsError LispVal) -- transformer
                -> RawPrimitive
typeTransformer t [x]     = t x
typeTransformer _ badArgs = throwError $ NumArgs 1 badArgs

charToNumber :: LispVal -> ThrowsError LispVal
charToNumber (Char c) = return . Number . fromIntegral $ ord c
charToNumber notChar  = throwError $ TypeMismatch "char" notChar

charToString :: LispVal -> ThrowsError LispVal
charToString (Char c) = return $ String [c]
charToString notChar  = throwError $ TypeMismatch "char" notChar

listToString :: LispVal -> ThrowsError LispVal
listToString (List chars) = String <$> mapchars chars where
    mapchars :: [LispVal] -> ThrowsError String
    mapchars []              = return []
    mapchars (Char c : cs) = (c :) <$> mapchars cs
    mapchars (notChar : _)   = throwError $ TypeMismatch "char" notChar
listToString notList      = throwError $ TypeMismatch "list" notList

numberToChar :: LispVal -> ThrowsError LispVal
numberToChar (Number n) = return . Char . chr $ fromIntegral n
numberToChar notNum     = throwError $ TypeMismatch "number" notNum

numberToString :: LispVal -> ThrowsError LispVal
numberToString (Number n) = return . String $ show n
numberToString notNum     = throwError $ TypeMismatch "number" notNum

stringToList :: LispVal -> ThrowsError LispVal
stringToList (String s) = return . List $ map Char s
stringToList notStr     = throwError $ TypeMismatch "string" notStr

stringToNumber :: LispVal -> ThrowsError LispVal
stringToNumber (String s) = let parsed = reads s in
    if null parsed || snd (head parsed) /= ""
    then return $ Bool False
    else return . Number . fst $ head parsed 
stringToNumber notStr     = throwError $ TypeMismatch "string" notStr
