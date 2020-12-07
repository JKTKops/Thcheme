module Primitives.Unwrappers where

import LispVal (LispVal (..), LispErr(TypeMismatch))
import EvaluationMonad (EM, throwError)

unwrapNum :: LispVal -> EM Integer
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapStr :: LispVal -> EM String
unwrapStr (String s) = return s
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapChar :: LispVal -> EM Char
unwrapChar (Char c) = return c
unwrapChar notChar  = throwError $ TypeMismatch "char" notChar

unwrapBool :: LispVal -> EM Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool
