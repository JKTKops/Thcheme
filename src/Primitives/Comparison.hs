{-# LANGUAGE ExistentialQuantification #-}
module Primitives.Comparison (rawPrimitives) where

import Control.Monad (liftM)
import Control.Monad.Except (throwError, catchError)
import Data.Char (ord, chr, toLower)

import Types
import Primitives.Bool (boolBinop)
import Primitives.Unwrappers

rawPrimitives = typeSpecific
             ++ [ (name, RPrim 2 eqf)
                | (name, eqf) <- [("eq?", eqv), ("eqv?", eqv), ("equal?", equal)]
                ]

typeSpecific = [ primGen builtin
               | (PrimBuilder primGen) <- primBuilders, builtin <- builtinComparisons
               ]

builtinComparisons :: Ord a => [(String, a -> a -> Bool)]
builtinComparisons = [ ("=", (==))
                     , ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     ]

data PrimBuilder = forall a. Ord a =>
                      PrimBuilder ((String, a -> a -> Bool) -> (String, RawPrimitive))

primBuilders = [PrimBuilder makeNumPrim, PrimBuilder makeStrPrim, PrimBuilder makeCharPrim]
  where makePrim :: String -- type name
                 -> Bool   -- appends '?' if False, nothing if True
                 -> ((a -> a -> Bool) -> RawPrimitive)
                 -> (String, a -> a -> Bool)
                 -> (String, RawPrimitive)
        makePrim name isNum primGen (opName, op) =
            (name ++ opName ++ (if isNum then "" else "?"), primGen op)
        makeNumPrim :: (String, Integer -> Integer -> Bool) -> (String, RawPrimitive)
        makeNumPrim = makePrim "" True numBoolBinop
        makeStrPrim = makePrim "string" False strBoolBinop
        makeCharPrim = makePrim "char" False charBoolBinop

-- boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> RawPrimitive

numBoolBinop :: (Integer -> Integer -> Bool) -> RawPrimitive
numBoolBinop = boolBinop unwrapNum
strBoolBinop :: (String -> String -> Bool) -> RawPrimitive
strBoolBinop = boolBinop unwrapStr
charBoolBinop :: (Char -> Char -> Bool) -> RawPrimitive
charBoolBinop = boolBinop unwrapChar

-- EQUIVALENCE FUNCTIONS
-- TODO some notion of function equivalence?
eqv :: RBuiltin
eqv [Bool x, Bool y]                   = return . Bool $ x == y
eqv [Number x, Number y]               = return . Bool $ x == y
eqv [String s, String t]               = return . Bool $ s == t
eqv [Atom x, Atom y]                   = return . Bool $ x == y
eqv [DottedList xs x, DottedList ys y] =
                           eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys]                 = return . Bool
    $ length xs == length ys && all pairEqv (zip xs ys) where
        pairEqv (x, y) = case eqv [x, y] of
            Left err         -> False
            Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgs                                = throwError $ NumArgs 2 badArgs

coerceNum :: LispVal -> ThrowsError Integer
coerceNum (Number n) = return n
coerceNum (String s) = let parsed = reads s in
    if null parsed
    then throwError $ TypeMismatch "number" $ String s
    else return . fst $ head parsed
coerceNum (Char c)   = return . fromIntegral $ ord c
coerceNum notNum     = throwError $ TypeMismatch "number" notNum

coerceStr :: LispVal -> ThrowsError String
coerceStr (String s) = return s
coerceStr (Number n) = return $ show n
coerceStr (Char c)   = return $ pure c
coerceStr (Bool b)   = return $ let s = show b in toLower (head s) : tail s
coerceStr notStr     = throwError $ TypeMismatch "string" notStr

coerceChar :: LispVal -> ThrowsError Char
coerceChar (Char c)   = return c
coerceChar (String s) =
    if length s == 1
    then return $ head s
    else throwError $ Default "" -- This error is always caught
coerceChar (Number n) = return . chr $ fromIntegral n
coerceChar notChar    = throwError $ TypeMismatch "char" notChar

coerceBool :: LispVal -> ThrowsError Bool
coerceBool (Bool b) = return b
coerceBool notBool  = throwError $ TypeMismatch "boolean" notBool

data Coercer = forall a. Eq a => Coercer (LispVal -> ThrowsError a)

coerceEquals :: LispVal -> LispVal -> Coercer -> ThrowsError Bool
coerceEquals x y (Coercer coercer) =
    do coercedx <- coercer x
       coercedy <- coercer y
       return $ coercedx == coercedy
    `catchError` const (return False)

equal :: RBuiltin
equal [DottedList xs x, DottedList ys y] =
    equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List xs, List ys]                 =
    return . Bool
    $ length xs == length ys && all pairEqv (zip xs ys) where
        pairEqv (x, y) = case equal [x, y] of
            Left err         -> False
            Right (Bool val) -> val

equal [x, y] = do
    equalCoerced <- or <$> mapM (coerceEquals x y)
                    [ Coercer coerceNum
                    , Coercer coerceStr
                    , Coercer coerceChar
                    , Coercer coerceBool ]
    primEqv      <- eqv [x, y]
    return . Bool $ (equalCoerced || let (Bool x) = primEqv in x)
equal badArgs = throwError $ NumArgs 2 badArgs
