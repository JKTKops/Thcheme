module Primitives.Unwrappers where

import Val (Val (..), LispErr(TypeMismatch))
import EvaluationMonad (EM, throwError)

unwrapNum :: Val -> EM Integer
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapStr :: Val -> EM String
unwrapStr (String s) = return s
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapChar :: Val -> EM Char
unwrapChar (Char c) = return c
unwrapChar notChar  = throwError $ TypeMismatch "char" notChar

unwrapBool :: Val -> EM Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool
