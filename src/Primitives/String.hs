{-# LANGUAGE LambdaCase #-}
module Primitives.String (rawPrimitives, macros) where

import Control.Monad.Except

import Types
import Evaluation
import EvaluationMonad

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("string", primString)
                , ("string-ref", primStringRef)
                , ("string-length", primStringLen)
                ]

macros :: [(String, Macro)]
macros = [ ("string-set!", primStringSet)
         ]

primString :: RawPrimitive
primString = RPrim 0 $ fmap String . go
  where go :: [LispVal] -> ThrowsError String
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

primStringSet :: Macro
primStringSet = Macro 3 $ \case
    args@(Atom _ : _) -> updateWith helper args
    args              -> helper args
  where helper :: [LispVal] -> EM LispVal
        helper args = do
            let (head : tail) = args
            argVals <- mapM eval tail
            case head : argVals of
                [String s, Number i, Char c] -> case fromIntegral i of
                    n | n `elem` [0..length s - 1] ->
                        let (pre, _:post) = splitAt n s in
                        return . String $ pre ++ (c:post)
                      | otherwise ->
                        throwError . Default $ "String index out of bounds: " ++ show n
                [String _, Number _, notChr] -> throwError $ TypeMismatch "char" notChr
                [String _, notNum, _] -> throwError $ TypeMismatch "number" notNum
                [notStr, _, _] -> throwError $ TypeMismatch "string" notStr
                badArgs -> throwError $ NumArgs 3 badArgs

primStringLen :: RawPrimitive
primStringLen = RPrim 1 $ \case
    [String s] -> return . Number . fromIntegral $ length s
    [notStr]   -> throwError $ TypeMismatch "string" notStr
    badArgs    -> throwError $ NumArgs 1 badArgs
