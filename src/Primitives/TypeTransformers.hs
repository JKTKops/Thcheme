module Primitives.TypeTransformers (primitives) where

import Data.Char (chr, ord)
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.Except (throwError)

import Val
import EvaluationMonad
import Types.Unwrappers (unwrapChar, unwrapExactInteger, unwrapStr)

primitives :: [Primitive]
primitives = [ typeTransformer name transform 
             | (name, transform) <-
                 [ ("char->number", charToNumber)
                 , ("char->string", charToString)
                 , ("list->string", listToString)
                 , ("list->vector", listToVector)
                 , ("number->string", numberToString)
                 , ("number->char", numberToChar)
                 , ("string->symbol", stringToSymbol)
                 , ("string->list", stringToList)
                 , ("string->number", stringToNumber)
                 , ("symbol->string", symbolToString)
                 , ("vector->list", vectorToList)
                 ]
             ]

typeTransformer :: Symbol -> (Val -> EM Val) -- transformer
                -> Primitive
typeTransformer name t = Prim name (Exactly 1) $
  \[x] -> t x

charToNumber :: Val -> EM Val
charToNumber (Char c) = return . makeBignum . fromIntegral $ ord c
charToNumber notChar  = throwError $ TypeMismatch "char" notChar

charToString :: Val -> EM Val
charToString (Char c) = String <$> newRef (T.singleton c)
charToString notChar  = throwError $ TypeMismatch "char" notChar

listToString :: Val -> EM Val
listToString v = do
    lst <- getListOrError v
    fmap String $ mapchars lst >>= newRef . pack
  where
    mapchars = mapM unwrapChar

listToVector :: Val -> EM Val
listToVector v = do
    vals <- getListOrError v
    liftIO $ Vector <$> V.thaw (V.fromList vals)

numberToChar :: Val -> EM Val
numberToChar v = do
  i <- unwrapExactInteger v
  return $ Char $ chr $ fromIntegral i

stringToSymbol :: Val -> EM Val
stringToSymbol val = Symbol <$> unwrapStr val

symbolToString :: Val -> EM Val
symbolToString (Symbol s) = pure $ IString $ symbolName s
symbolToString v = throwError $ TypeMismatch "symbol" v

-- R7RS is unclear if these strings should be mutable. I'm guessing that in
-- the absence of an explicit suggestion, we should make them mutable.
numberToString :: Val -> EM Val
numberToString (Number n) = String <$> newRef (pack $ show n)
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
stringToList val = unwrapStr val >>= makeMutableList . map Char . unpack

-- TODO [r7rs]
-- needs access to the new parser to read numbers

-- | Pretty broken, only accepts strings containing integers in base 10.
stringToNumber :: Val -> EM Val
stringToNumber str = do
    s <- unwrapStr str
    let parsed = reads $ unpack s
    if null parsed || snd (head parsed) /= ""
      then return $ Bool False
      else return . makeBignum . fst $ head parsed

vectorToList :: Val -> EM Val
vectorToList (IVector v) = makeMutableList $ V.toList v
vectorToList (Vector v) = do
    imm <- liftIO $ V.freeze v
    makeMutableList $ V.toList imm
vectorToList notVec = throwError $ TypeMismatch "vector" notVec
