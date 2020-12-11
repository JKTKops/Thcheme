{-# LANGUAGE LambdaCase #-}
module Primitives.List (primitives) where

import Control.Monad (replicateM)

import Val
import EvaluationMonad

primitives :: [Primitive]
primitives =
  [ isListP
  , consP
  , listP
  , appendP
  , nullP
  , setCarP
  , setCdrP
  ]
  ++ cxrCompositions [1..4]

isListP :: Primitive
isListP = Prim "list?" 1 $ \case
  [x] -> Bool <$> isListSH x
  bad -> throwError $ NumArgs 1 bad

consP :: Primitive
consP = Prim "cons" 2 $ \case
    [x, y]  -> consSSS x y
    badArgs -> throwError $ NumArgs 2 badArgs

carB :: Builtin
carB [IList (x:_)]         = return x
carB [IDottedList (x:_) _] = return x
carB [PairPtr pair]        = carRS pair
carB [badArg] = throwError $ TypeMismatch "pair" badArg
carB badArgs  = throwError $ NumArgs 1 badArgs

cdrB :: Builtin
cdrB [IList (_:xs)]         = return $ IList xs
cdrB [IDottedList [_] x]    = return x
cdrB [IDottedList (_:xs) x] = return $ IDottedList xs x
cdrB [PairPtr pair]         = cdrRS pair
cdrB [badArg] = throwError $ TypeMismatch "pair" badArg
cdrB badArgs  = throwError $ NumArgs 1 badArgs

cxrCompositions :: [Int] -> [Primitive]
cxrCompositions ns = do
    len         <- ns
    combination <- replicateM len carAndCdr
    let (n, b) = foldr1 combine combination
    return $ Prim ("c" ++ n ++ "r") 1 b
  where
    carAndCdr = [("a", carB), ("d", cdrB)]
    -- 'cadr' does cdr, then car, so we use left fish instead of right
    combine (c1, m1) (c2, m2) = (c1 ++ c2, m1 . single <=< m2)
    single x = [x]

listP :: Primitive
listP = Prim "list" 0 makeMutableList

appendP :: Primitive
appendP = Prim "append" 1 aux
  where 
    -- aux will never be called with 0 arguments since arity is 1+
    aux [x] = return x
    aux xs = do
        (lists, last) <- walk xs
        makeImproperMutableList (concat lists) last
    
    walk :: [Val] -> EM ([[Val]], Val)
    walk [x] = return ([], x)
    walk (x:xs) = do
        requireList x
        ~(lists, last) <- walk xs
        FList fx <- freezeList x
        return (fx:lists, last)

nullP :: Primitive
nullP = Prim "null?" 1 $ \case
  [Nil] -> return $ Bool True
  [IList []] -> return $ Bool True -- AAAAAAAH
  [_]   -> return $ Bool False
  badArgs -> throwError $ NumArgs 1 badArgs

setCarP :: Primitive
setCarP = Prim "set-car!" 2 $ \case
  [PairPtr pair, v] -> setCarRSS pair v
  [badPair, _]
    | isImmutablePair badPair -> throwError $ SetImmutable "pair"
    | otherwise -> throwError $ TypeMismatch "pair" badPair
  badArgs -> throwError $ NumArgs 2 badArgs

setCdrP :: Primitive
setCdrP = Prim "set-cdr!" 2 $ \case
  [PairPtr pair, v] -> setCdrRSS pair v
  [badPair, _]
    | isImmutablePair badPair -> throwError $ SetImmutable "pair"
    | otherwise -> throwError $ TypeMismatch "pair" badPair
  badArgs -> throwError $ NumArgs 2 badArgs
