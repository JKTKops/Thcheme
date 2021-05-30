{-# LANGUAGE LambdaCase #-}
module Primitives.List (primitives) where

import Control.Monad (replicateM)

import Val
import EvaluationMonad

primitives :: [Primitive]
primitives =
  [ isListP
  , isCircularListP
  , consP
  , listP
  , appendP
  , nullP
  , setCarP
  , setCdrP
  ]
  ++ cxrCompositions [1..4]

isListP :: Primitive
isListP = Prim "list?" (Exactly 1) $ \[x] -> Bool <$> isListSH x

isCircularListP :: Primitive
isCircularListP = Prim "circular-list?" (Exactly 1) $
  \[x] -> Bool <$> testCircularList x

consP :: Primitive
consP = Prim "cons" (Exactly 2) $ \[x, y]  -> consSSS x y

carB :: Builtin
carB [pair@Pair{}]  = carPS pair
carB [pair@IPair{}] = carPS pair
carB [badArg] = throwError $ TypeMismatch "pair" badArg
carB _ = panic "car arity"

cdrB :: Builtin
cdrB [pair@Pair{}]  = cdrPS pair
cdrB [pair@IPair{}] = cdrPS pair
cdrB [badArg] = throwError $ TypeMismatch "pair" badArg
cdrB _ = panic "cdr arity"

cxrCompositions :: [Int] -> [Primitive]
cxrCompositions ns = do
    len         <- ns
    combination <- replicateM len carAndCdr
    let (n, b) = foldr1 combine combination
    return $ Prim ("c" ++ n ++ "r") (Exactly 1) b
  where
    carAndCdr = [("a", carB), ("d", cdrB)]
    -- 'cadr' does cdr, then car, so we use left fish instead of right
    combine (c1, m1) (c2, m2) = (c1 ++ c2, m1 . single <=< m2)
    single x = [x]

listP :: Primitive
listP = Prim "list" (AtLeast 0) makeMutableList

appendP :: Primitive
appendP = Prim "append" (AtLeast 0) aux
  where 
    aux [] = return Nil
    aux [x] = return x
    aux xs = do
        (lists, last) <- walk xs
        makeImproperMutableList (concat lists) last
    
    walk :: [Val] -> EM ([[Val]], Val)
    walk [] = panic "append arity"
    walk [x] = return ([], x)
    walk (x:xs) = do
        requireList x
        ~(lists, last) <- walk xs
        FList fx <- freezeList x
        return (fx:lists, last)

nullP :: Primitive
nullP = Prim "null?" (Exactly 1) $ \case
  [Nil] -> return $ Bool True
  [_]   -> return $ Bool False
  _ -> panic "null arity"

setCarP :: Primitive
setCarP = Prim "set-car!" (Exactly 2) $ \[pair, v] -> setCarSSS pair v

setCdrP :: Primitive
setCdrP = Prim "set-cdr!" (Exactly 2) $ \[pair, v] -> setCdrSSS pair v
