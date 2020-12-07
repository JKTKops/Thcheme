{-# LANGUAGE LambdaCase #-}
module Primitives.TypeTransformers (primitives) where

import Data.Char (chr, ord)
import Data.Array
import Control.Monad.Except (throwError)

import LispVal
import EvaluationMonad

primitives :: [Primitive]
primitives = [ typeTransformer name transform 
             | (name, transform) <-
                 [ ("char->number", charToNumber)
                 , ("char->string", charToString)
                 , ("list->string", listToString)
                 , ("list->vector", listToVector)
                 , ("number->string", numberToString)
                 , ("number->char", numberToChar)
                 , ("string->list", stringToList)
                 , ("string->number", stringToNumber)
                 , ("vector->list", vectorToList)
                 ]
             ]

typeTransformer :: String -> (LispVal -> EM LispVal) -- transformer
                -> Primitive
typeTransformer name t = Prim name 1 $ \case
    [x]     -> t x
    badArgs -> throwError $ NumArgs 1 badArgs

charToNumber :: LispVal -> EM LispVal
charToNumber (Char c) = return . Number . fromIntegral $ ord c
charToNumber notChar  = throwError $ TypeMismatch "char" notChar

charToString :: LispVal -> EM LispVal
charToString (Char c) = return $ String [c]
charToString notChar  = throwError $ TypeMismatch "char" notChar

listToString :: LispVal -> EM LispVal
listToString (List chars) = String <$> mapchars chars where
    mapchars :: [LispVal] -> EM String
    mapchars []              = return []
    mapchars (Char c : cs) = (c :) <$> mapchars cs
    mapchars (notChar : _)   = throwError $ TypeMismatch "char" notChar
listToString notList = throwError $ TypeMismatch "list" notList

listToVector :: LispVal -> EM LispVal
listToVector (List vals) = return . Vector $
                             listArray (0, fromIntegral $ length vals - 1) vals
listToVector notList = throwError $ TypeMismatch "list" notList

numberToChar :: LispVal -> EM LispVal
numberToChar (Number n) = return . Char . chr $ fromIntegral n
numberToChar notNum     = throwError $ TypeMismatch "number" notNum

numberToString :: LispVal -> EM LispVal
numberToString (Number n) = return . String $ show n
numberToString notNum     = throwError $ TypeMismatch "number" notNum

stringToList :: LispVal -> EM LispVal
stringToList (String s) = return . List $ map Char s
stringToList notStr     = throwError $ TypeMismatch "string" notStr

stringToNumber :: LispVal -> EM LispVal
stringToNumber (String s) = let parsed = reads s in
    if null parsed || snd (head parsed) /= ""
    then return $ Bool False
    else return . Number . fst $ head parsed
stringToNumber notStr     = throwError $ TypeMismatch "string" notStr

vectorToList :: LispVal -> EM LispVal
vectorToList (Vector arr) = return . List $ elems arr
vectorToList notVec = throwError $ TypeMismatch "vector" notVec
