module Primitives.Unwrappers where

import Val ( Val (..), Number (..), RealNumber (..)
           , LispErr(TypeMismatch), getExactInteger )
import EvaluationMonad (EM, throwError, readRef)

unwrapNum :: Val -> EM Number
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapRealNum :: Val -> EM RealNumber
unwrapRealNum (Number (Real r)) = pure r
unwrapRealNum notRealNum = throwError $ TypeMismatch "real number" notRealNum

unwrapExactInteger :: Val -> EM Integer
unwrapExactInteger v = case getExactInteger v of
  Just i -> return i
  Nothing -> throwError $ TypeMismatch "exact integer" v

unwrapSymbol :: Val -> EM String
unwrapSymbol (Symbol s) = pure s
unwrapSymbol notSymbol = throwError $ TypeMismatch "symbol" notSymbol

unwrapStr :: Val -> EM String
unwrapStr (IString s) = return s
unwrapStr (String ref) = readRef ref
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapChar :: Val -> EM Char
unwrapChar (Char c) = return c
unwrapChar notChar  = throwError $ TypeMismatch "char" notChar

unwrapBool :: Val -> EM Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool
