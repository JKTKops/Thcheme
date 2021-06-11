-- | Recursive-descent r7rs Scheme parser, built on top of Alex
-- and an incremental stream architecture.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Parser.Parser (readDatum, parseDatum, parseDatumSeq) where

import Control.Monad.Trans (lift)
import Control.Monad.ST.Trans
import Data.Complex (Complex(..))
import Data.Ratio (numerator, denominator, (%))
import Text.Parsec as P hiding (runParser, token, tokenPrim, eof)
import Text.Parsec.Error

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
      -- this will put a literal TokEOF in the stream at the end.
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

liftAlex :: Alex a -> Parser s a
liftAlex = lift . lift


-- ----------------------------------------------------------------------------
-- Entrypoints

-- | Read the next datum parseable from the given port.
readDatum :: String -> Port -> Either String Val
readDatum = runParser datum

-- | Parse an input port that contains exactly one datum.
parseDatum :: String -> Port -> Either String Val
parseDatum = runParser $ datum <* eof

parseDatumSeq :: String -> Port -> Either String [Val]
parseDatumSeq = runParser $ datumSeq <* eof
  -- enforce that we reach the end of the stream, since we only
  -- use this entrypoint to parse whole source files.

-- ----------------------------------------------------------------------------
-- Token parsers

-- | this is like 'Text.Parsec.Prim.tokenPrim', except that 'posFromTok' should
-- give the position of the _matched_ token, not the next one. We need
-- something like this because we absolutely cannot allow a lookahead token.
-- This has the side effect of leaving Parsec's internally stored position
-- on the previously-lexed token, instead of the start of the next one like
-- parsec is expecting. This is /more reasonable/ behavior, at least in our
-- case, and results in better error messages in 'validateByte'.
--
-- The previous 'SourcePos' is available to the position-extraction function
-- so that it can build a 'SourcePos' with parsec's 'setXXX' functions, since
-- the constructor is not exposed. It's unlikely that you'll actually need to
-- use the previous 'SourcePos' in a meaningful way.
tokenPrim :: Stream s m t
          => (t -> String) -> (SourcePos -> t -> SourcePos) -> (t -> Maybe a)
          -> ParsecT s u m a
tokenPrim showTok posFromTok test = mkPT $
  \(State input pos usr) -> do
    r <- uncons input
    pure $ case r of
      Nothing -> Empty $ pure $ P.Error $ unexpectError "" pos
      Just (c,rest) -> let newpos = posFromTok pos c
                       in case test c of
        Nothing -> Empty $ pure $ P.Error $ unexpectError (showTok c) newpos
        Just x -> let newState = State rest newpos usr
                  in newpos `seq` newState `seq`
                     Consumed $ pure $ Ok x newState (newErrorUnknown newpos)
  where
    unexpectError msg pos = newErrorMessage (SysUnExpect msg) pos

token :: (Token -> Maybe a) -> Parser s a
token match = tokenPrim displayLexeme movePos match'
  where
    movePos oldPos (L (AlexPn _addr line col) _)
      = setSourceColumn (setSourceLine oldPos line) col
    
    match' (L _ tok) = match tok

tokenWithPos :: (Token -> Maybe a) -> Parser s (a, AlexPosn)
tokenWithPos match = tokenPrim displayLexeme movePos match'
  where
    movePos oldPos (L (AlexPn _addr line col) _)
      = setSourceColumn (setSourceLine oldPos line) col
    
    match' (L pos tok) = (,pos) <$> match tok

displayLexeme :: Lexeme -> String
displayLexeme (L _ t) = "'" ++ show t ++ "'"

eof :: Parser s ()
eof = token $ \case
  TokEOF -> Just ()
  _other -> Nothing

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

dot :: Parser s ()
dot = token $ \case
  TokDot -> Just ()
  _other -> Nothing

datumCommentStart :: Parser s ()
datumCommentStart = do
  token $ \case
    TokDatumComment -> Just ()
    _other -> Nothing
  liftAlex enterDatumComment

datumCommentEnd :: Parser s ()
datumCommentEnd = liftAlex exitDatumComment

startVector :: Parser s ()
startVector = token $ \case
  TokLVector -> Just ()
  _other -> Nothing

startByteVector :: Parser s ()
startByteVector = token $ \case
  TokLByteVector -> Just ()
  _other -> Nothing

startLabelDef :: Parser s Int
startLabelDef = do
  (lbl, pos) <- tokenWithPos $ \case
    TokLabelDef lbl -> Just lbl
    _other -> Nothing
  liftAlex $ startDef pos lbl
  return lbl

labelReference :: Parser s Val
labelReference = do
  lbl <- token $ \case
    TokLabelRef lbl -> Just lbl
    _other -> Nothing
  liftAlex $ lookupLabel lbl

-- | Parses a label reference, but additionally ensures
-- that the parser's internal 'defining_label' field does
-- not match the parsed label. If it does, raises an error
-- (at the Alex level).
safeLabelReference :: Parser s Val
safeLabelReference = do
  lbl <- token $ \case
    TokLabelRef lbl -> Just lbl
    _other -> Nothing
  liftAlex $ testRef lbl >> lookupLabel lbl

-- ----------------------------------------------------------------------------
-- Core parser

-- trying to be a little bit more aggressive about having a simple grammar
-- with parsec, since we don't have to satisfy shift/reduce conflicts.
-- We still have to stratify label definitions, to check well-definedness.

datum :: Parser s Val
datum = simpleDatum
        <|> compoundDatum
        <|> definedDatum
        <|> labelReference
        <|> (datumComment >> datum)
        <?> "datum"

-- This parser is not (fully) incremental. It will give up on EOF,
-- of course, but if the input stream contains only some data
-- followed by non-data text, it will try to read one "token"
-- out of the non-data text before returning. This is unavoidable,
-- otherwise it can't know when to quit.
datumSeq :: Parser s [Val]
datumSeq = many datum

datumComment :: Parser s ()
datumComment = do
  datumCommentStart
  _ <- datum
  datumCommentEnd

compoundDatum :: Parser s Val
compoundDatum = (prefixSugar <$> sugarToken <*> datum)
                <|> list
                <|> vector

list :: Parser s Val
list = labeled "list" $ bracketed listContents

listContents :: Parser s Val
listContents = do
  prefix <- many datum
  maybeDot <- optionMaybe $ dot >> datum
  case maybeDot of
    Nothing -> return $ makeImmutableList prefix
    Just d  -> return $ makeImproperImmutableList prefix d

vector :: Parser s Val
vector = normalVector <|> byteVector

normalVector :: Parser s Val
normalVector = labeled "vector" $ do
  startVector
  contents <- many datum
  closeParen
  return $ makeVector contents

byteVector :: Parser s Val
byteVector = labeled "bytevector" $ do
  startByteVector
  bytes <- many byte
  closeParen
  return $ makeByteVector bytes

closeParen :: Parser s ()
closeParen = token $ \case
  TokRParen -> Just ()
  _other -> Nothing

byte :: Parser s Byte
byte = labeled "byte" $ do
  (num, pos) <- tokenWithPos $ \case
    TokNumber n -> Just n
    _other -> Nothing
  validateByte pos num

definedDatum :: Parser s Val
definedDatum = do
  lbl <- startLabelDef
  _ <- fail $ "At the moment, cyclic references will hang, so Thcheme"
              ++ " refuses all label definitions."
  val <- defDatum
  liftAlex $ do
    endDef
    unlessDatumComment $ defineLabel lbl val
    return val

defDatum :: Parser s Val
defDatum = simpleDatum
           <|> safeLabelReference
           <|> (prefixSugar <$> sugarToken <*> datum)
           <|> vector
           <|> defList

-- | like 'list', but if the list contains only a dot, and that
-- dot is a reference to a label that is defined as the list itself,
-- then raises an error as the label is ill-defined.
defList :: Parser s Val
-- Note 'listContents -> . d' is a production of the grammar, but it
-- doesn't matter as long as we test for that case first.
defList = labeled "list" $ bracketed $ (dot >> defDatum) <|> listContents
  
-- ----------------------------------------------------------------------------
-- Utilities for constructing/validating Vals

labeled :: String -> Parser s a -> Parser s a
labeled = flip label

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

prefixSugar :: Token -> Val -> Val
prefixSugar tok v = makeImmutableList [Symbol (desugar tok), v]
  where
    desugar TokQuote     = "quote"
    desugar TokBackquote = "quasiquote"
    desugar TokComma     = "unquote"
    desugar TokCommaAt   = "unquote-splicing"
    desugar _ = panic "desugar: not a sugar token!"

validateByte :: AlexPosn -> Numeric -> Parser s Byte
validateByte p n = case n of
  LitInteger i -> checkSize i
  LitRational r
    | denominator r == 1 -> checkSize $ numerator r
    | otherwise -> boom
  LitComplex real imag -> do checkExactZero imag
                             validateByte p real
  _other -> boom
  where
    checkSize :: Integer -> Parser s Byte
    checkSize i
      | i >= 0 && i <= 255 = return $ fromInteger i
      | otherwise = boom

    checkExactZero :: Numeric -> Parser s ()
    checkExactZero (LitInteger 0) = pure ()
    checkExactZero (LitRational r)
      | r == 0 % 1 = pure ()
      | otherwise = boom
    checkExactZero _ = boom

    boom :: Parser s a
    boom = fail $ show n ++ " is not an exact byte value"
