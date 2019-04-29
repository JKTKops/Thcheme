module RawPrimitives.Unwrappers where

import Control.Monad.Except (throwError)

import LispVal

unwrapNum :: LispVal -> ThrowsError Integer
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapStr :: LispVal -> ThrowsError String
unwrapStr (String s) = return s
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapChar :: LispVal -> ThrowsError Char
unwrapChar (Char c) = return c
unwrapChar notChar  = throwError $ TypeMismatch "char" notChar

unwrapBool :: LispVal -> ThrowsError Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool
