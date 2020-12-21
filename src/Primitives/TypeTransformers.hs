{-# LANGUAGE LambdaCase #-}
module Primitives.TypeTransformers (primitives) where

import Data.Char (chr, ord)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
import Control.Monad.Except (throwError)

import Val
import EvaluationMonad
import Primitives.String hiding (primitives)

primitives :: [Primitive]
primitives = [ typeTransformer name transform 
             | (name, transform) <-
                 [ ("char->number", charToNumber)
                 , ("char->string", charToString)
                 , ("list->string", listToString)
                 , ("list->vector", listToVector)
                 , ("number->string", numberToString)
                 , ("number->char", numberToChar)
                 , ("string->list", stringToList)
                 , ("string->number", stringToNumber)
                 , ("vector->list", vectorToList)
                 ]
             ]

typeTransformer :: String -> (Val -> EM Val) -- transformer
                -> Primitive
typeTransformer name t = Prim name (Exactly 1) $
  \[x] -> t x

charToNumber :: Val -> EM Val
charToNumber (Char c) = return . Number . fromIntegral $ ord c
charToNumber notChar  = throwError $ TypeMismatch "char" notChar

charToString :: Val -> EM Val
charToString (Char c) = String <$> newRef [c]
charToString notChar  = throwError $ TypeMismatch "char" notChar

listToString :: Val -> EM Val
listToString v = do
    lst <- getListOrError v
    fmap String $ mapchars lst >>= newRef
  where
    mapchars []              = return []
    mapchars (Char c : cs) = (c :) <$> mapchars cs
    mapchars (notChar : _)   = throwError $ TypeMismatch "char" notChar

listToVector :: Val -> EM Val
listToVector v = do
    vals <- getListOrError v
    liftIO $ Vector <$> V.thaw (V.fromList vals)

numberToChar :: Val -> EM Val
numberToChar (Number n) = return . Char . chr $ fromIntegral n
numberToChar notNum     = throwError $ TypeMismatch "number" notNum

-- R7RS is unclear if these strings should be mutable. I'm guessing that in
-- the absence of an explicit suggestion, we should make them mutable.
numberToString :: Val -> EM Val
numberToString (Number n) = String <$> newRef (show n)
numberToString notNum     = throwError $ TypeMismatch "number" notNum

-- TODO: the 'start' and 'end' arguments should probably be added here,
-- otherwise we'd allocate a too-large mutable list and trim it. There are
-- obvious problems with that; the procedure should be efficient for a
-- large string if '(- end start)' is small.

-- We can implement stringCopyHHRH and use that to implement all of
-- stringCopyP, substringP, stringToListP, etc. Knowing that stringCopyHHRH
-- doesn't allocate a fresh (scheme) string guarantees that the implementation
-- of stringToListP will allocate a fresh list without allocating an
-- intermediate string.
stringToList :: Val -> EM Val
stringToList val
  | stringSH val = unwrapStringPH val >>= makeMutableList . map Char
  | otherwise = throwError $ TypeMismatch "string" val

stringToNumber :: Val -> EM Val
stringToNumber str
  | stringSH str = do
    s <- unwrapStringPH str
    let parsed = reads s
    if null parsed || snd (head parsed) /= ""
      then return $ Bool False
      else return . Number . fst $ head parsed
  | otherwise = throwError $ TypeMismatch "string" str

vectorToList :: Val -> EM Val
vectorToList (IVector v) = makeMutableList $ V.toList v
vectorToList (Vector v) = do
    imm <- liftIO $ V.freeze v
    makeMutableList $ V.toList imm
vectorToList notVec = throwError $ TypeMismatch "vector" notVec
