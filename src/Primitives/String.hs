{-# LANGUAGE LambdaCase #-}
module Primitives.String 
  ( primitives
  
    -- * haskell-level utilities
  , unwrapStringPH, stringSH
  ) where

import Control.Monad.IO.Class
import Data.IORef (readIORef, writeIORef)
import Data.List (splitAt) -- if we switch to text, drop this
import Data.Functor (($>))

import Val
import EvaluationMonad

-- TODO: use Data.CaseInsensitive to add the -foldcase functions

primitives :: [Primitive]
primitives = [stringP, stringLengthP, stringRefP, stringSetP]

-- This implementation of strings is not great, using linear time to get the
-- length and to string-ref etc. However at least for now this seems fine.
-- Data.Text would be a relatively easy improvement. Possibly the best version
-- would be to use an unboxed vector of characters, since Scheme strings have
-- a fixed length. Unclear. Perhaps you want to try them and benchmark?

stringP :: Primitive
stringP = Prim "string" (AtLeast 0) stringB

stringB :: Builtin
stringB [] = String <$> newRef "" -- fast track empty list
stringB cs = checkChars cs >> String <$> newRef (extract cs)
  where
    checkChars [] = pure ()
    checkChars (Char c : cs) = checkChars cs
    checkChars (notChar:_) = throwError $ TypeMismatch "char" notChar

    extract [] = ""
    extract (Char c : cs) = c : extract cs

stringLengthP :: Primitive
stringLengthP = Prim "string-length" (Exactly 1) $ \case
  [val] | stringSH val -> Number . toInteger . length <$> unwrapStringPH val
        | otherwise -> throwError $ TypeMismatch "string" val

stringRefP :: Primitive
stringRefP = Prim "string-ref" (Exactly 2) $ \case
  [string, num]
    | stringSH string, Number k <- num
    -> do str <- unwrapStringPH string
          let len = toInteger $ length str
          if k < 0 || k >= len
            then throwError $ Default 
              $ "string index out of bounds: " ++ show k
            else return $ Char $ str !! fromInteger k

    | stringSH string -> throwError $ TypeMismatch "integer" num
    | otherwise -> throwError $ TypeMismatch "string" string

stringSetP :: Primitive
stringSetP = Prim "string-set!" (Exactly 3) $ \case
  [String ref, Number k, Char c] -> do
    str <- readRef ref
    let len = toInteger $ length str
    if k < 0 || k >= len
      then throwError $ Default
        $ "string index out of bounds: " ++ show k
      else let (pre, _ : post) = splitAt (fromInteger k) str
               adjusted = pre ++ c : post
           in writeRef ref adjusted $> Char c
  [IString _, Number _, Char _] -> throwError $ SetImmutable "string"
  [string, num, char]
    | not $ stringSH string -> throwError $ TypeMismatch "string" string
    -- if num actually is a number, char must not be a character.
    -- otherwise one of the above patterns would've matched.
    | Number _ <- num -> throwError $ TypeMismatch "character" char
    | otherwise       -> throwError $ TypeMismatch "integer" num

-------------------------------------------------------
-- Haskell-level utilities
-------------------------------------------------------

stringSH :: Val -> Bool
stringSH String{} = True
stringSH IString{} = True
stringSH _ = False

unwrapStringPH :: MonadIO m => Val -> m String
unwrapStringPH (IString s) = return s
unwrapStringPH (String ref) = liftIO $ readIORef ref
