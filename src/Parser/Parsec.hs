-- | Recursive-descent r7rs Scheme parser, built on top of Alex
-- and an incremental stream architecture.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Parser.Parsec where

import Control.Monad.Trans (lift)
import Control.Monad.ST.Trans
import Data.Complex (Complex(..))
import Data.Ratio (numerator, denominator, (%))
import Text.Parsec hiding (runParser, token)

import Val
import EvaluationMonad (panic)
import Parser.Monad hiding (token)
import Parser.Lexer

-- Unfortunately, we have to define the parts of the monad that go on top
-- of Alex here, and not in Monad.hs. We could get /most/ of the way there
-- by reorganizing tokens and lexemes, but we can't move 'alexMonadScan'
-- there because it requires things defined in Lexer.hs.
-- I guess in an ideal world, we'd define all this stuff in Monad.hs and
-- get it to compile with a Lexer.hs-boot file.

-- | A lazily-constructed, cached, incremental 'Lexeme' stream type.
-- Haskell's own laziness cannot (obviously, perhaps there is a way)
-- be used to construct a stream of 'Lexeme's because we do not know when
-- the stream will end until the parser decides that it is done parsing.
newtype S s = S (STRef s (Maybe (Maybe (Lexeme, S s))))

unconsS :: S s -> M s (Maybe (Lexeme, S s))
unconsS (S ref) = do
  cache <- readSTRef ref
  case cache of
    Just res -> return res
    Nothing -> do
      lx <- lift alexMonadScan
      restS <- S <$> newSTRef Nothing
      let res = Just (lx, restS)
      writeSTRef ref $ Just res
      return res

instance Stream (S s) (M s) Lexeme where
  uncons = unconsS

-- | The monad underlying ParsecT.
type M s = STT s Alex

-- | The parsec parsing monad.
type Parser s = ParsecT (S s) () (M s)

runParser :: (forall s. Parser s a) -> String -> Port -> Either String a
runParser p label inputPort 
  = case runAlex' alex label inputPort of
    Right (Left parseError) -> Left $ show parseError
    Right (Right v) -> Right v
    Left lexError -> Left lexError
  where
    alex = runSTT $ do
    initialS <- S <$> newSTRef Nothing
    runParserT p () label initialS

-- ----------------------------------------------------------------------------
-- Token parsers

token :: (Token -> Maybe a) -> Parser s a
token match = tokenPrim show movePos match'
  where
    movePos oldPos (L (AlexPn _addr line col) _) _stream
      = setSourceColumn (setSourceLine oldPos line) col
    
    match' (L _ tok) = match tok

simpleDatum :: Parser s Val
simpleDatum = token $ \case
  TokSymbol sym -> Just $ Symbol $ pack sym
  TokNumber num -> Just $ mkNumber num
  TokBool b     -> Just $ Bool b
  TokChar c     -> Just $ Char c
  TokString str -> Just $ IString $ pack str
  _other        -> Nothing

bracketed :: Parser s a -> Parser s a
bracketed inside = do
  rightBracket <- leftBracket
  r <- inside
  rightBracket
  return r
  where
    leftBracket = token $ \case
      TokLParen -> mkRight $ \case
        TokRParen -> Just ()
        _other    -> Nothing
      TokLBracket -> mkRight $ \case
        TokRBracket -> Just ()
        _other      -> Nothing
      TokLBrace -> mkRight $ \case
        TokRBrace -> Just ()
        _other    -> Nothing
      _other -> Nothing
    mkRight = return . token

sugarToken :: Parser s Token
sugarToken = token $ \case
  TokQuote -> Just TokQuote
  TokComma -> Just TokComma
  TokCommaAt -> Just TokCommaAt
  TokBackquote -> Just TokBackquote 
  _other -> Nothing

-- ----------------------------------------------------------------------------
-- Utilities for constructing/validating Vals

mkNumber :: Numeric -> Val
mkNumber (LitInteger i)  = Number $ Real $ Bignum i
mkNumber (LitRational r) = Number $ Real $ Ratnum r
mkNumber (LitFloating f) = Number $ Real $ Flonum f
mkNumber (LitComplex l r) = Number $ Complex $ 
  numericToNumber l :+ numericToNumber r

numericToNumber :: Numeric -> RealNumber
numericToNumber (LitInteger i)  = Bignum i
numericToNumber (LitRational r) = Ratnum r
numericToNumber (LitFloating f) = Flonum f
numericToNumber LitComplex{} = panic "numericToNumber: nested complex"
