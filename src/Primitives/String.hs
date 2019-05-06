{-# LANGUAGE LambdaCase #-}
module Primitives.String (rawPrimitives) where

import Control.Monad.Except

import Types

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("string", primString)
             , ("string-ref", primStringRef)
             , ("string-length", primStringLen)
             ]

primString :: RawPrimitive
primString = RPrim 0 $ \chars -> String <$> go chars
  where go :: [LispVal] -> ThrowsError [Char]
        go []              = return []
        go (Char c : rest) = (c:) <$> go rest
        go (notChar:_)     = throwError $ TypeMismatch "char" notChar

primStringRef :: RawPrimitive
primStringRef = RPrim 2 $ \case
    [String s, Number i] -> case fromIntegral i of
        n | n `elem` [0..length s - 1] -> return . Char $ s !! n
          | otherwise -> throwError . Default $ "String index out of bounds: " ++ show n
    [String _, notNum] -> throwError $ TypeMismatch "number" notNum
    [notStr, _] -> throwError $ TypeMismatch "string" notStr
    badArgs     -> throwError $ NumArgs 2 badArgs

primStringLen :: RawPrimitive
primStringLen = RPrim 1 $ \case
    [String s] -> return . Number . fromIntegral $ length s
    [notStr]   -> throwError $ TypeMismatch "string" notStr
    badArgs    -> throwError $ NumArgs 1 badArgs
