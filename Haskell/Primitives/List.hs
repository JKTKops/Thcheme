module Primitives.List
    ( car
    , cdr
    , cons
    ) where

import Control.Monad.Except (throwError)

import LispVal

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
