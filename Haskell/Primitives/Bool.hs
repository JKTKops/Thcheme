module Primitives.Bool 
    ( boolBinop
    , boolAnd
    , boolOr
    , boolNot
    ) where

import Control.Monad.Except (throwError)

import LispVal
import Primitives.Unwrappers (unwrapBool)

boolBinop :: (LispVal -> ThrowsError a) -- unwrapper
          -> (a -> a -> Bool) -- binary boolean operation
          -> [LispVal] -- args; expects 2
          -> ThrowsError LispVal -- output LispVal:Bool
boolBinop unwrapper op [left, right] = do
    left' <- unwrapper left
    right' <- unwrapper right
    return . Bool $ left' `op` right'
boolBinop _ _ args = throwError $ NumArgs 2 args

boolBoolBinop = boolBinop unwrapBool

boolAnd = boolBoolBinop (&&)
boolOr  = boolBoolBinop (||)

boolNot :: [LispVal] -> ThrowsError LispVal
boolNot [(Bool False)] = return $ Bool True
boolNot [_]            = return $ Bool False
boolNot badArgs        = throwError $ NumArgs 1 badArgs
