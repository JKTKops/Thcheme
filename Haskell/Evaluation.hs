{-# LANGUAGE ExistentialQuantification #-}
module Evaluation (eval) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (liftM, mapM)

import LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _)   = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe 
                    (throwError $ NotFunction "Unrecognized primitive function" func)
                    ($ args) 
                $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             , ("symbol?", guardOneArg isSymbol)
             , ("string?", guardOneArg isString)
             , ("number?", guardOneArg isNumber)
             , ("boolean?", guardOneArg isBool)
             , ("list?", guardOneArg isList)
             , ("pair?", guardOneArg isPair)
             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))
             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))
             , ("string=?", strBoolBinop (==))
             , ("string<?", strBoolBinop (<))
             , ("string>?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)
             , ("eq?", eqv)
             , ("eqv?", eqv)
             , ("equal?", equal)
             ]

numericBinop :: (Integer -> Integer -> Integer)
             -> [LispVal]
             -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unwrapNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -- unwrapper
          -> (a -> a -> Bool) -- binary boolean operation
          -> [LispVal] -- args; expects 2
          -> ThrowsError LispVal -- output LispVal:Bool
boolBinop unwrapper op [left, right] = do
    left' <- unwrapper left
    right' <- unwrapper right
    return . Bool $ left' `op` right'
boolBinop _ _ args = throwError $ NumArgs 2 args

numBoolBinop = boolBinop unwrapNum
strBoolBinop = boolBinop unwrapStr
boolBoolBinop = boolBinop unwrapBool

unwrapNum :: LispVal -> ThrowsError Integer
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapStr :: LispVal -> ThrowsError String
unwrapStr (String s) = return s
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapBool :: LispVal -> ThrowsError Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool

guardOneArg :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
guardOneArg func args@[x] = return $ func x
guardOneArg _ args        = throwError $ NumArgs 1 args

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _        = Bool False

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList _        = Bool False

isPair :: LispVal -> LispVal
isPair (List []) = Bool False
isPair (List _)  = Bool True
isPair (DottedList _ _) = Bool True

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]        = return x
car [DottedList(x:xs) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgs              = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgs               = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]            = return $ List [x]
cons [x, List xs]            = return . List $ x:xs
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x, y]                  = return $ DottedList [x] y
cons badArgs                 = throwError $ NumArgs 2 badArgs

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)]                   = return . Bool $ x == y
eqv [(Number x), (Number y)]               = return . Bool $ x == y
eqv [(String s), (String t)]               = return . Bool $ s == t
eqv [(Atom x), (Atom y)]                   = return . Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] =
                           eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List xs), (List ys)]                 = return . Bool
    $ (length xs == length ys) && (all pairEqv $ zip xs ys) where 
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
    else return . fst $ parsed !! 0

coerceStr :: LispVal -> ThrowsError String
coerceStr (String s) = return s
coerceStr (Number n) = return $ show n
coerceStr (Bool b)   = return $ show b
coerceStr notStr     = throwError $ TypeMismatch "string" notStr

coerceBool :: LispVal -> ThrowsError Bool
coerceBool (Bool b) = return b
coerceBool notBool  = throwError $ TypeMismatch "boolean" notBool

data Coercer = forall a. Eq a => Coercer (LispVal -> ThrowsError a)

coerceEquals :: LispVal -> LispVal -> Coercer -> ThrowsError Bool
coerceEquals x y (Coercer coercer) = 
    do
        coercedx <- coercer x
        coercedy <- coercer y
        return $ coercedx == coercedy
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do
    equalCoerced <- liftM or $ mapM (coerceEquals x y)
                    [ Coercer coerceNum
                    , Coercer coerceStr
                    , Coercer coerceBool ]
    primEqv      <- eqv [x, y]
    return . Bool $ (equalCoerced || let (Bool x) = primEqv in x)
equal badArgs = throwError $ NumArgs 2 badArgs
