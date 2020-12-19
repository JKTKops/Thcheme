{-# LANGUAGE LambdaCase #-}
module Primitives.TypeTransformers (primitives) where

import Data.Char (chr, ord)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
import Control.Monad.Except (throwError)

import Val
import EvaluationMonad

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
typeTransformer name t = Prim name 1 $ \case
    [x]     -> t x
    badArgs -> throwError $ NumArgs 1 badArgs

charToNumber :: Val -> EM Val
charToNumber (Char c) = return . Number . fromIntegral $ ord c
charToNumber notChar  = throwError $ TypeMismatch "char" notChar

charToString :: Val -> EM Val
charToString (Char c) = return $ String [c]
charToString notChar  = throwError $ TypeMismatch "char" notChar

listToString :: Val -> EM Val
listToString v = do
    lst <- getListOrError v
    String <$> mapchars lst
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

numberToString :: Val -> EM Val
numberToString (Number n) = return . String $ show n
numberToString notNum     = throwError $ TypeMismatch "number" notNum

-- TODO: the 'start' and 'end' arguments should probably be added here,
-- otherwise we'd allocate a too-large mutable list and trim it. There are
-- obvious problems with that; the procedure should be efficient for a
-- large string if '(- end start)' is small.

-- perhaps a reasonable way to manage stuff like that would be to use a
-- new naming convention for all these confusing functions flying around.
-- an H means the type is a Haskell type, i.e. Integer or PairObj.
-- an S means the type is a scheme object (aka 'Val').
--   S is more distinct than V.
-- an R means the type is 'Ref H' for some H type.
-- a  P must be alone, and means the type of the function is 'Builtin'.
--
-- xyzSS is a function from a scheme object to another scheme object.
--   i.e. stringCopyHHSS :: Int -> Int -> Val -> EM Val
-- xyzSH is a function from a scheme object to a haskell object.
--   i.e. unwrapNumSH :: Val -> EM Integer
-- xyzSR is a function from a scheme object to a reference to a haskell object
--   where we use "haskell object" to mean PairObj, StringObj etc.;
--   that is, the versions of Scheme objects that would be found after chasing
--   all the indirections from the object itself.
--   i.e. stringCopySR :: Val -> EM (Ref StringObj)
--
-- then we implement stringCopyHHRH and use that to implement all of
-- stringCopyP, substringP, stringToListP, etc. Knowing that stringCopyHHRH
-- doesn't allocate a fresh (scheme) string guarantees that the implementation
-- of stringToListP will allocate a fresh list without allocating an
-- intermediate string.
stringToList :: Val -> EM Val
stringToList (String s) = makeMutableList $ map Char s
stringToList notStr     = throwError $ TypeMismatch "string" notStr

stringToNumber :: Val -> EM Val
stringToNumber (String s) = let parsed = reads s in
    if null parsed || snd (head parsed) /= ""
    then return $ Bool False
    else return . Number . fst $ head parsed
stringToNumber notStr     = throwError $ TypeMismatch "string" notStr

vectorToList :: Val -> EM Val
vectorToList (IVector v) = makeMutableList $ V.toList v
vectorToList (Vector v) = do
    imm <- liftIO $ V.freeze v
    makeMutableList $ V.toList imm
vectorToList notVec = throwError $ TypeMismatch "vector" notVec
