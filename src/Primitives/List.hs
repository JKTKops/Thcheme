{-# LANGUAGE LambdaCase #-}
module Primitives.List (primitives) where

import Control.Monad (replicateM)
import Data.Foldable (foldrM)
import Data.Functor (($>))

import LispVal
import EvaluationMonad

primitives :: [Primitive]
primitives =
  [ isListPrim
  , consPrim
  , listPrim
  , appendPrim
  , nullPrim
  , setCarPrim
  , setCdrPrim
  ]
  ++ cxrCompositions [1..4]

isListPrim :: Primitive
isListPrim = Prim "list?" 1 $ \case
  [x] -> Bool <$> isList x
  bad -> throwError $ NumArgs 1 bad

consPrim :: Primitive
consPrim = Prim "cons" 2 $ \case
    [x, y]  -> cons x y
    badArgs -> throwError $ NumArgs 2 badArgs

carBuiltin :: Builtin
carBuiltin [IList (x:_)]         = return x
carBuiltin [IDottedList (x:_) _] = return x
carBuiltin [Pair c _d]            = readRef c
carBuiltin [badArg] = throwError $ TypeMismatch "pair" badArg
carBuiltin badArgs  = throwError $ NumArgs 1 badArgs

cdrBuiltin :: Builtin
cdrBuiltin [IList (_:xs)]         = return $ IList xs
cdrBuiltin [IDottedList [_] x]    = return x
cdrBuiltin [IDottedList (_:xs) x] = return $ IDottedList xs x
cdrBuiltin [Pair _c d]            = readRef d
cdrBuiltin [badArg] = throwError $ TypeMismatch "pair" badArg
cdrBuiltin badArgs  = throwError $ NumArgs 1 badArgs

cxrCompositions :: [Int] -> [Primitive]
cxrCompositions ns = do
    len         <- ns
    combination <- replicateM len carAndCdr
    let (n, b) = foldr1 combine combination
    return $ Prim ("c" ++ n ++ "r") 1 b
  where
    carAndCdr = [("a", carBuiltin), ("d", cdrBuiltin)]
    -- 'cadr' does cdr, then car, so we use left fish instead of right
    combine (c1, m1) (c2, m2) = (c1 ++ c2, m1 . single <=< m2)
    single x = [x]

listPrim :: Primitive
listPrim = Prim "list" 0 makeMutableList

appendPrim :: Primitive
appendPrim = Prim "append" 1 aux
  where 
    -- aux will never be called with 0 arguments since arity is 1+
    aux [x] = return x
    aux xs = do
        (lists, last) <- walk xs
        foldrM cons last $ concat lists
    
    walk :: [LispVal] -> EM ([[LispVal]], LispVal)
    walk [x] = return ([], x)
    walk (x:xs) = do
        requireList x
        ~(lists, last) <- walk xs
        IList fx <- freezeList x
        return (fx:lists, last)

nullPrim :: Primitive
nullPrim = Prim "null?" 1 $ \case
  [Nil] -> return $ Bool True
  [_]   -> return $ Bool False
  badArgs -> throwError $ NumArgs 1 badArgs

setCarPrim :: Primitive
setCarPrim = Prim "set-car!" 2 $ \case
  [Pair c _d, v] -> writeRef c v $> v
  [badPair, _]
    | isImmutablePair badPair -> throwError $ SetImmutable "pair"
    | otherwise -> throwError $ TypeMismatch "pair" badPair
  badArgs -> throwError $ NumArgs 2 badArgs

setCdrPrim :: Primitive
setCdrPrim = Prim "set-cdr!" 2 $ \case
  [Pair _c d, v] -> writeRef d v $> v
  [badPair, _]
    | isImmutablePair badPair -> throwError $ SetImmutable "pair"
    | otherwise -> throwError $ TypeMismatch "pair" badPair
  badArgs -> throwError $ NumArgs 2 badArgs
