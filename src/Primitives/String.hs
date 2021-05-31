{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Primitives.String 
  ( primitives
  
    -- * haskell-level utilities
  , unwrapStringPH, stringSH
  ) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.IORef (readIORef)
import Data.Functor (($>))
import qualified Data.Text as T

import Val
import EvaluationMonad
import Primitives.Unwrappers (unwrapExactInteger)

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
stringB cs = checkChars cs >> String <$> newRef (pack $ extract cs)
  where
    checkChars [] = pure ()
    checkChars (Char{} : cs) = checkChars cs
    checkChars (notChar:_) = throwError $ TypeMismatch "char" notChar

    extract [] = ""
    extract (Char c : cs) = c : extract cs
    extract _ = panic "string.extract: impossible non-char"

stringLengthP :: Primitive
stringLengthP = Prim "string-length" (Exactly 1) $ \case
  [val] | stringSH val -> makeBignum 
                          . toInteger
                          . T.length
                          <$> unwrapStringPH val
        | otherwise -> throwError $ TypeMismatch "string" val
  _ -> panic "stringLength arity"

stringRefP :: Primitive
stringRefP = Prim "string-ref" (Exactly 2) $ \case
  [string, num]
    | stringSH string
    -> do k <- unwrapExactInteger num
          str <- unwrapStringPH string
          let len = toInteger $ T.length str
          if k < 0 || k >= len
            then throwError $ Default 
              $ "string index out of bounds: " ++ show k
            else return $ Char $ str `T.index` fromInteger k
    | otherwise -> throwError $ TypeMismatch "string" string
  _ -> panic "stringRef arity"

stringSetP :: Primitive
stringSetP = Prim "string-set!" (Exactly 3) $ \case
  [String ref, getExactInteger -> Just k, Char c] -> do
    str <- readRef ref
    let len = toInteger $ T.length str
    if k < 0 || k >= len
      then throwError $ Default
        $ "string index out of bounds: " ++ show k
      else let (pre, T.tail -> post) = T.splitAt (fromInteger k) str
               adjusted = pre <> T.singleton c <> post
           in writeRef ref adjusted $> Char c
  [IString _, Number _, Char _] -> throwError $ SetImmutable "string"
  [string, num, char]
    | not $ stringSH string -> throwError $ TypeMismatch "string" string
    -- if num actually is a number, char must not be a character.
    -- otherwise one of the above patterns would've matched.
    | Number _ <- num -> throwError $ TypeMismatch "character" char
    | otherwise       -> throwError $ TypeMismatch "exact integer" num
  _ -> panic "stringSet arity"

-------------------------------------------------------
-- Haskell-level utilities
-------------------------------------------------------

stringSH :: Val -> Bool
stringSH String{} = True
stringSH IString{} = True
stringSH _ = False

unwrapStringPH :: MonadIO m => Val -> m Text
unwrapStringPH (IString s) = return s
unwrapStringPH (String ref) = liftIO $ readIORef ref
unwrapStringPH _ = panic "unwrapString: not a string"
