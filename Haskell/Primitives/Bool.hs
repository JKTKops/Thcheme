module Primitives.Bool (primitives, boolBinop) where

import Control.Monad.Except (throwError)

import LispVal
import Primitives.Unwrappers (unwrapBool)

primitives = [ ("&&", boolAnd)
             , ("||", boolOr)
             , ("not", boolNot)
             ]

boolBinop :: (LispVal -> ThrowsError a) -- unwrapper
          -> (a -> a -> Bool) -- binary boolean operation
          -> RawPrimitive
boolBinop unwrapper op [left, right] = do
    left' <- unwrapper left
    right' <- unwrapper right
    return . Bool $ left' `op` right'
boolBinop _ _ args = throwError $ NumArgs 2 args

boolBoolBinop = boolBinop unwrapBool

boolAnd = boolBoolBinop (&&)
boolOr  = boolBoolBinop (||)

boolNot :: RawPrimitive 
boolNot [Bool False]   = return $ Bool True
boolNot [_]            = return $ Bool False
boolNot badArgs        = throwError $ NumArgs 1 badArgs
