{-# LANGUAGE LambdaCase #-}
module Primitives.List (rawPrimitives, macros) where

import Prelude hiding (sequence)
import Control.Monad (liftM)
import Control.Monad.Except (throwError, (>=>))
import Control.Monad.Writer (Writer, writer, runWriter, tell)

import Types
import Evaluation (eval)
import EvaluationMonad (updateWith)

rawPrimitives = [ ("list", listOp)
                , ("cons", cons)
                , ("append", appendOp)
                , ("null?", nullOp)
                ] ++
                [ (name, RPrim 1 func)
                | (name, func) <- compositions [1..4] [("a", car), ("d", cdr)]
                ]

macros = [ ("set-car!", setCar)
         , ("set-cdr!", setCdr)
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
                                         -- See note [car/cdr names]
                                         writer (val:list, log `mappend` iden)

{-
NOTE [car/cdr names]
`cadr` performs `cdr`, then performs `car`.
Because of this, we take the computation result to be a list
of car/cdr operations in the order we intend to execute them,
but put the identifier _after_ the existing log so that they
will appear in reverse order.
-}

listOp :: RawPrimitive
listOp = RPrim 0 $ return . List

appendOp :: RawPrimitive
appendOp = RPrim 0 $ fmap List . worker
  where worker :: [LispVal] -> ThrowsError [LispVal]
        worker []             = return []
        worker (List [] : ls) = worker ls
        worker (List xs : ls) = (xs ++) <$> worker ls
        worker (notList : _)  = throwError $ TypeMismatch "list" notList

nullOp :: RawPrimitive
nullOp = RPrim 1 $ \case
    [List []] -> return $ Bool True
    [List _]  -> return $ Bool False
    [notList] -> throwError $ TypeMismatch "list" notList
    badArgs   -> throwError $ NumArgs 1 badArgs

setCar :: Macro
setCar = Macro 2 $ \args -> case args of
    (Atom _ : _) -> updateWith helper args
    _            -> helper args
  where helper :: [LispVal] -> EM LispVal
        helper [List (_ : cdr), form] = do
            form' <- eval form
            return $ List (form' : cdr)
        helper [DottedList (_ : cdr1) cdr2, form] = do
            form' <- eval form
            return $ DottedList (form' : cdr1) cdr2
        helper [List [], _] = throwError $ TypeMismatch "pair" (List [])
        helper [notList, _] = throwError $ TypeMismatch "pair" notList
        helper badArgs      = throwError $ NumArgs 2 badArgs

setCdr :: Macro
setCdr = Macro 2 $ \args -> case args of
    (Atom _ : _) -> updateWith helper args
    _            -> helper args
  where helper :: [LispVal] -> EM LispVal
        helper [List (car : _), form] = do
            form' <- eval form
            return $ case form' of
                List cdr -> List (car : cdr)
                DottedList cdr1 cdr2 -> DottedList (car : cdr1) cdr2
                _ -> DottedList [car] form'
        helper [DottedList (car : _) _, form] = do
            form' <- eval form
            return $ case form' of
                List cdr -> List (car : cdr)
                DottedList cdr1 cdr2 -> DottedList (car : cdr1) cdr2
                _ -> DottedList [car] form'
        helper [List [], _] = throwError $ TypeMismatch "pair" (List [])
        helper [notList, _] = throwError $ TypeMismatch "pair" notList
        helper badArgs      = throwError $ NumArgs 2 badArgs