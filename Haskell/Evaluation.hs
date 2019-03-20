module Evaluation (eval) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad (mapM)

import LispVal

eval :: LispVal -> ThrowsError LispVal
eval val@(Atom _)   = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
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
boolBinop unwrapper op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        left <- unwrapper $ args !! 0
        right <- unwrapper $ args !! 1
        return . Bool $ left `op` right

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
