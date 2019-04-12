module Primitives.List (primitives) where

import Prelude hiding (sequence)
import Control.Monad (liftM)
import Control.Monad.Except (throwError, (>=>))
import Control.Monad.Writer (Writer, writer, runWriter, tell)

import LispVal ( LispVal (..)
               , LispErr ( TypeMismatch
                         , NumArgs)
               , RawPrimitive
               , ThrowsError)

primitives = [ ("list", listOp)
             , ("cons", cons)
             , ("null?", nullOp)
             ]
             ++ compositions [1..4] [("a", car), ("d", cdr)]

car :: RawPrimitive
car [List (x:xs)]        = return x
car [DottedList(x:xs) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgs              = throwError $ NumArgs 1 badArgs

cdr :: RawPrimitive
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgs               = throwError $ NumArgs 1 badArgs

cons :: RawPrimitive
cons [x, List []]            = return $ List [x]
cons [x, List xs]            = return . List $ x:xs
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x, y]                  = return $ DottedList [x] y
cons badArgs                 = throwError $ NumArgs 2 badArgs

compositions :: [Int] -> [(String, RawPrimitive)] -> [(String, RawPrimitive)]
compositions nums = map writerToPrim . logChooseN nums
    where writerToPrim :: Writer String [RawPrimitive] -> (String, RawPrimitive)
          writerToPrim w = let (prims, log) = runWriter w
                           in ("c" ++ log ++ "r", foldr1 sequence prims)

-- The fully general type signature is
-- sequence :: (Monad m, Applicative f) => (a -> m b) -> (f b -> m c) -> a -> mc
sequence :: RawPrimitive -- ^ First primitive to execute
         -> RawPrimitive -- ^ Use the result of first primitive as argument to this primitive
         -> RawPrimitive
sequence f g = f >=> g . return

logChooseN :: (Monoid m) => [Int] -> [(m, a)] -> [Writer m [a]]
logChooseN ns as = do
    num <- ns 
    foldr (.) id (replicate num $ addChoice as) [return []]
    where
        addChoice :: (Monoid m) => [(m, a)] -> [Writer m [a]] -> [Writer m [a]]
        addChoice as ws = do (iden, val) <- as
                             w <- ws
                             return $ do let (list, log) = runWriter w
                                         writer (val:list, iden `mappend` log)

listOp :: RawPrimitive
listOp = return . List

nullOp :: RawPrimitive
nullOp [List []] = return $ Bool True
nullOp [List _]  = return $ Bool False
nullOp [notList] = throwError $ TypeMismatch "list" notList
nullOp badArgs   = throwError $ NumArgs 1 badArgs
