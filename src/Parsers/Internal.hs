module Parsers.Internal where

import Text.Parsec hiding (Error, try, spaces)
import Text.Parsec.Char (octDigit, hexDigit)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Token as Tok

import Data.Char (digitToInt)
import Data.Vector (fromList)
import Control.Monad.Except (throwError)

import Types (ThrowsError)
import Val

labeledReadOrThrow :: String -> Parser a -> String -> ThrowsError a
labeledReadOrThrow label parser input = case parse parser label input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow = labeledReadOrThrow "Thcheme"

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where style = emptyDef { Tok.commentStart = "#|"
                         , Tok.commentEnd   = "|#"
                         , Tok.commentLine  = ";"
                         , Tok.identStart   = letter <|> symbol
                         , Tok.identLetter  = letter <|> digit <|> symbol
                         }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

identifier :: Parser String
identifier = Tok.identifier lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

anyBraces :: Parser a -> Parser a
anyBraces p = parens p
          <|> Tok.braces lexer p
          <|> Tok.brackets lexer p

decimal, hexadecimal, octal :: Parser Integer
decimal     = Tok.decimal lexer
hexadecimal = Tok.hexadecimal lexer
octal       = Tok.octal lexer

float :: Parser Double
float = Tok.float lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

parseExpr :: Parser Val
parseExpr = lexeme $
             (try parseNumber <?> "number")
             -- atom names can start with #\ too so we need an @try@
         <|> (try parseChar <?> "character literal")
         <|> (try parseVector <?> "vector")
         <|> (parseAtom <?> "symbol")
         <|> (parseString <?> "string")
         <|> (parseQuoted <?> "quote form")
         <|> (lexeme (anyBraces parseListlike) <?> "list")

symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:\\/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

delim :: Parser ()
delim = notFollowedBy $ alphaNum <|> symbol

parseString :: Parser Val
parseString = IString <$> stringLiteral

parseAtom :: Parser Val
parseAtom = do
    atom <- identifier
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser Val
parseNumber = lexeme $ do
    sign <- optionMaybe $ char '-' <|> char '+'
    prefix <- parseRadixPrefix <|> return "#d"
    num <- Number <$> case prefix of
        "#b" -> number 2 (char '0' <|> char '1')
        "#o" -> number 8 octDigit
        "#d" -> number 10 digit -- float case goes here :)
        "#h" -> number 16 hexDigit
    delim
    case sign of
        Nothing  -> return num
        Just '+' -> return num
        Just '-' -> let Number n = num in return . Number $ negate n

  where parseRadixPrefix :: Parser String
        parseRadixPrefix = do
            hash <- char '#'
            prefix <- oneOf "bodh"
            return [hash, prefix]

        number base baseDigit = do
            digits <- many1 baseDigit
            let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
            seq n (return n)

parseChar :: Parser Val
parseChar = lexeme $ do
    _prefix <- string "#\\"
    char   <- do try $ string "space"
                 return ' '
              <|> (do try $ string "newline"
                      return '\n')
              <|> (do try $ string "carriage-return"
                      return '\r')
              <|> (do try $ string "tab"
                      return '\t')
              <|> try (do c <- anyChar
                          delim
                          return c)
              <?> "char literal"
    return $ Char char

parseVector :: Parser Val
parseVector = lexeme $ do
    char '#'
    exprs <- parens $ many parseExpr
    return . IVector $ fromList exprs

parseListlike :: Parser Val
parseListlike = do
    init <- many parseExpr
    mlast <- optionMaybe $ lexeme (char '.') >> parseExpr
    case (init, mlast) of
        (list, Nothing) -> return $ makeImmutableList list
        ([], Just e)  -> return e
        (init, Just dot) -> return $ makeImproperImmutableList init dot

-- these are provided strictly for back-compat with the testing code.
-- them being separately defined leads to exponential parsing time!
parseList, parseDottedList :: Parser Val
parseList = parseListlike
parseDottedList = parseListlike

parseQuoted :: Parser Val
parseQuoted = lexeme $ foldr1 (<|>) parsers
  where parsers = map (\sym -> do
            try $ string sym
            let macro = case sym of
                    "'"  -> "quote"
                    "`"  -> "quasiquote"
                    ","  -> "unquote"
                    ",@" -> "unquote-splicing"
            x <- parseExpr
            return $ makeImmutableList [Atom macro, x]) ["'", "`", ",@", ","]
