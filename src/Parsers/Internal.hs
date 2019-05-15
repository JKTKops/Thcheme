module Parsers.Internal where

import Text.Parsec hiding (Error, try, spaces)
import Text.Parsec.Char (octDigit, hexDigit)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Char (digitToInt)
import Data.Array (listArray)
import Control.Monad (liftM, guard)
import Control.Monad.Except (throwError, liftIO)
import Numeric (readInt, readOct, readHex)

import Types

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
                         , Tok.opLetter     = oneOf "'`,@"
                         , Tok.reservedOpNames = ["'", "`", ",", ",@"]
                         }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens p = Tok.parens lexer p

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

parseExpr :: Parser LispVal
parseExpr = lexeme $
             try parseNumber
         <|> try parseChar -- #\atom is a valid name for an atom
         <|> try parseVector
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> lexeme (anyBraces $ try parseDottedList <|> parseList)

symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:\\/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

delim :: Parser ()
delim = notFollowedBy $ alphaNum <|> symbol

parseString :: Parser LispVal
parseString = String <$> stringLiteral

parseAtom :: Parser LispVal
parseAtom = do
    atom <- identifier
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
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

parseChar :: Parser LispVal
parseChar = lexeme $ do
    prefix <- string "#\\"
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

parseVector :: Parser LispVal
parseVector = lexeme $ do
    char '#'
    exprs <- parens $ many parseExpr
    return . Vector . listArray (0, fromIntegral $ length exprs - 1) $ exprs

parseList :: Parser LispVal
parseList = List <$> many parseExpr -- sepBy parseExpr (skipMany space)

parseDottedList :: Parser LispVal
parseDottedList = do
    init <- many parseExpr
    last <- lexeme (char '.') >> parseExpr
    case (init, last) of
        ([], _)         -> return last
        (init, List ls) -> return . List $ init ++ ls
        (init, val)     -> return $ DottedList init val

parseQuoted :: Parser LispVal
parseQuoted = lexeme $ foldr1 (<|>) parsers
  where parsers = map (\sym -> do
            reservedOp sym
            let macro = case sym of
                    "'"  -> "quote"
                    "`"  -> "quasiquote"
                    ","  -> "unquote"
                    ",@" -> "unquote-splicing"
            x <- parseExpr
            return $ List [Atom macro, x]) ["'", "`", ",", ",@"]
