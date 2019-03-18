module Parsers (readExpr) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM, guard)
import Control.Monad.Except (throwError)
import Numeric (readInt, readOct, readHex)

import LispVal (LispVal (..), LispErr (..), ThrowsError)


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "Lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr = try parseNumber 
         <|> parseAtom 
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (fmap return notEscape <|> escape <|> fmap return space)
    char '"'
    return $ String . concat $ x where
        notEscape = noneOf "\\\"\0\n\r\v\t\b\f"
        escape = do
            d <- char '\\'
            c <- oneOf "\\\"nrvtbf"
            return [c]

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

data Base = Bin | Oct | Dec | Hex deriving (Eq, Show, Enum, Bounded)

parseNumber :: Parser LispVal
parseNumber = do
    prefix <- parseRadixPrefix <|> return "#d"
    many1 (digit <|> oneOf "abcdef") >>= case prefix of
        "#b" -> parseBin
        "#o" -> parseOct
        "#d" -> parseDec
        "#h" -> parseHex
    where
        parseRadixPrefix :: Parser String
        parseRadixPrefix = do
            hash <- char '#'
            prefix <- oneOf "bodh"
            return [hash, prefix]

        parseBin :: String -> Parser LispVal
        parseBin = liftNumber . readBin

        readBin :: String -> Integer
        readBin = fromRead . (readInt 2 (`elem` "01") (read . pure))

        parseOct :: String -> Parser LispVal
        parseOct = liftNumber . fromRead . readOct

        parseDec :: String -> Parser LispVal
        parseDec s = do
            let parses = reads s
            guard $ length parses /= 0
            liftNumber . fst $ head parses

        parseHex :: String -> Parser LispVal
        parseHex = liftNumber . fromRead . readHex

        fromRead :: [(Integer, String)] -> Integer
        fromRead [] = 0
        fromRead (x:_) = fst x

        liftNumber :: Integer -> Parser LispVal
        liftNumber = return . Number

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
