{-# LANGUAGE ExistentialQuantification #-}
module Primitives.Comparison (primitives) where

import Control.Monad.Except (throwError, catchError)
import Data.Char (ord, chr, toLower)

import Types -- when the equivalence functions are updated to work properly,
             -- only import Val!
import Primitives.Bool (boolBinop)
import Primitives.Unwrappers

primitives :: [Primitive]
primitives = typeSpecific
          ++ [ Prim name 2 $ liftEither . eqf
             | (name, eqf) <- [("eq?", eqv), ("eqv?", eqv), ("equal?", equal)]
             ]

typeSpecific :: [Primitive]
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

-- | A wrapper over a function that converts a (Haskell) builtin comparison
-- function into a Primitive function. We use this to convert the Haskell
-- builtins into appropriate comparison functions for all Scheme types.
data PrimBuilder = forall a. Ord a =>
                      PrimBuilder ((String, a -> a -> Bool) -> Primitive)

primBuilders :: [PrimBuilder]
primBuilders = [ PrimBuilder makeNumPrim
               , PrimBuilder makeStrPrim
               , PrimBuilder makeCharPrim
               ]
  where makePrim :: String -- type name
                 -> Bool   -- appends '?' if False, nothing if True
                 -> (String -> (a -> a -> Bool) -> Primitive)
                 -> (String, a -> a -> Bool)
                 -> Primitive
        makePrim tyname isNum primGen (opName, op) = primGen primName op
          where primName = tyname ++ opName ++ suffix
                suffix | isNum = "?"
                       | otherwise = ""

        makeNumPrim = makePrim "" True numBoolBinop
        makeStrPrim = makePrim "string" False strBoolBinop
        makeCharPrim = makePrim "char" False charBoolBinop

-- boolBinop :: String -> (Val -> EM a) -> (a -> a -> Bool) -> Primitive

-- TODO: none of these satisfy r7rs, which says the predicate should be
-- satisfied pairwise along a whole list of elements; we should not use
-- 'boolBinop'.
numBoolBinop :: String -> (Integer -> Integer -> Bool) -> Primitive
numBoolBinop name = boolBinop name unwrapNum
strBoolBinop :: String -> (String -> String -> Bool) -> Primitive
strBoolBinop name = boolBinop name unwrapStr
charBoolBinop :: String -> (Char -> Char -> Bool) -> Primitive
charBoolBinop name = boolBinop name unwrapChar

-- EQUIVALENCE FUNCTIONS
-- TODO some notion of function equivalence?

-- TODO [r7rs]
-- I don't think this function satisfies r7rs currently. Left as TODO
-- because it's highly affected by shoving IORefs into Val.
-- Adjust as appropriate later. EM will be required.
eqv :: [Val] -> ThrowsError Val
eqv [Bool x, Bool y]                   = return . Bool $ x == y
eqv [Number x, Number y]               = return . Bool $ x == y
eqv [Char x, Char y]                   = return . Bool $ x == y
eqv [String s, String t]               = return . Bool $ s == t
eqv [Atom x, Atom y]                   = return . Bool $ x == y
eqv [IDottedList xs x, IDottedList ys y] =
                           eqv [IList $ xs ++ [x], IList $ ys ++ [y]]
eqv [IList xs, IList ys]                 = return . Bool
    $ length xs == length ys && all pairEqv (zip xs ys) where
        pairEqv (x, y) = case eqv [x, y] of
            Left _err        -> False
            Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgs                                = throwError $ NumArgs 2 badArgs

-- TODO: instead of defining coercers, we should just import TypeTransformers
-- and implement the standard properly.
-- furthermore, we aren't defining case-insensitive string/char comparisons.
-- Once we have char-foldcase and string-foldcase we should just call those
-- similar to how we'd call a type transformer.

coerceNum :: Val -> ThrowsError Integer
coerceNum (Number n) = return n
coerceNum (String s) = let parsed = reads s in
    if null parsed
    then throwError $ TypeMismatch "number" $ String s
    else return . fst $ head parsed
coerceNum (Char c)   = return . fromIntegral $ ord c
coerceNum notNum     = throwError $ TypeMismatch "number" notNum

coerceStr :: Val -> ThrowsError String
coerceStr (String s) = return s
coerceStr (Number n) = return $ show n
coerceStr (Char c)   = return $ pure c
coerceStr (Bool b)   = return $ let s = show b in toLower (head s) : tail s
coerceStr notStr     = throwError $ TypeMismatch "string" notStr

coerceChar :: Val -> ThrowsError Char
coerceChar (Char c)   = return c
coerceChar (String s) =
    if length s == 1
    then return $ head s
    else throwError $ Default "" -- This error is always caught
coerceChar (Number n) = return . chr $ fromIntegral n
coerceChar notChar    = throwError $ TypeMismatch "char" notChar

coerceBool :: Val -> ThrowsError Bool
coerceBool (Bool b) = return b
coerceBool notBool  = throwError $ TypeMismatch "boolean" notBool

data Coercer = forall a. Eq a => Coercer (Val -> ThrowsError a)

coerceEquals :: Val -> Val -> Coercer -> ThrowsError Bool
coerceEquals x y (Coercer coercer) =
    do coercedx <- coercer x
       coercedy <- coercer y
       return $ coercedx == coercedy
    `catchError` const (return False)

-- see the comment on eqv
equal :: [Val] -> ThrowsError Val
equal [IDottedList xs x, IDottedList ys y] =
    equal [IList $ xs ++ [x], IList $ ys ++ [y]]
equal [IList xs, IList ys]                 =
    return . Bool
    $ length xs == length ys && all pairEqv (zip xs ys) where
        pairEqv (x, y) = case equal [x, y] of
            Left _err        -> False
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
