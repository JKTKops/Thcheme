{-# LANGUAGE LambdaCase #-}
module RawPrimitives.List (primitives) where

import Prelude hiding (sequence)
import Control.Monad (liftM)
import Control.Monad.Except (throwError, (>=>))
import Control.Monad.Writer (Writer, writer, runWriter, tell)

import LispVal

primitives = [ ("list", listOp)
             , ("cons", cons)
             , ("null?", nullOp)
             ]
             ++ [ (name, RPrim 1 func)
                | (name, func) <- compositions [1..4] [("a", car), ("d", cdr)]
                ]

car :: RBuiltin
car [List (x:xs)]        = return x
car [DottedList(x:xs) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgs              = throwError $ NumArgs 1 badArgs

cdr :: RBuiltin
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgs               = throwError $ NumArgs 1 badArgs

cons :: RawPrimitive
cons = RPrim 2 $ \case
    [x, List []]            -> return $ List [x]
    [x, List xs]            -> return . List $ x:xs
    [x, DottedList xs last] -> return $ DottedList (x:xs) last
    [x, y]                  -> return $ DottedList [x] y
    badArgs                 -> throwError $ NumArgs 2 badArgs

compositions :: [Int] -> [(String, RBuiltin)] -> [(String, RBuiltin)]
compositions nums = map writerToPrim . logChooseN nums
    where writerToPrim :: Writer String [RBuiltin] -> (String, RBuiltin)
          writerToPrim w = let (prims, log) = runWriter w
                           in ("c" ++ log ++ "r", foldr1 sequence prims)

-- The fully general type signature is
-- sequence :: (Monad m, Applicative f) => (a -> m b) -> (f b -> m c) -> a -> mc
sequence :: RBuiltin -- ^ First primitive to execute
         -> RBuiltin -- ^ Use the result of first primitive as argument to this primitive
         -> RBuiltin
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
listOp = RPrim 0 $ return . List

appendOp :: RawPrimitive
appendOp = RPrim 2 $ \case
    [List a, List b] -> return . List $ a ++ b
    [List _, notList] -> throwError $ TypeMismatch "list" notList
    [notList, _]      -> throwError $ TypeMismatch "list" notList
    badArgs           -> throwError $ NumArgs 2 badArgs

nullOp :: RawPrimitive
nullOp = RPrim 1 $ \case
    [List []] -> return $ Bool True
    [List _]  -> return $ Bool False
    [notList] -> throwError $ TypeMismatch "list" notList
    badArgs   -> throwError $ NumArgs 1 badArgs
