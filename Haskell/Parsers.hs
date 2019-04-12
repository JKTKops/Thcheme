module Parsers (readExpr, readExprList, load) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Maybe (isNothing)
import Control.Monad (liftM, guard)
import Control.Monad.Except (throwError, liftIO)
import Numeric (readInt, readOct, readHex)

import LispVal (LispVal (..), LispErr (..)
               , IOThrowsError, ThrowsError, liftThrows)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "Thcheme" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

parseExpr :: Parser LispVal
parseExpr = try parseNumber 
         <|> try parseChar -- #\atom is a valid name for an atom 
         <|> parseAtom 
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:\\/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (fmap return notEscape <|> escape <|> fmap return space)
    char '"'
    return $ String . concat $ x where
        notEscape = noneOf "\\\"\n\r\t"
        escape = do
            d <- char '\\'
            c <- oneOf "\\\"nrt"
            return . pure $ case c of
                '\\' -> '\\'
                '"'  -> '"'
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    sign <- optionMaybe $ char '-'
    prefix <- parseRadixPrefix <|> return "#d"
    num <- many1 (digit <|> oneOf "abcdef") >>= case prefix of
        "#b" -> parseBin
        "#o" -> parseOct
        "#d" -> parseDec
        "#h" -> parseHex
    if isNothing sign
    then return num
    else case num of
        (Number n) -> return . Number $ negate n
    where
        parseRadixPrefix :: Parser String
        parseRadixPrefix = do
            hash <- char '#'
            prefix <- oneOf "bodh"
            return [hash, prefix]

        parseBin :: String -> Parser LispVal
        parseBin = liftNumber . readBin

        readBin :: String -> Integer
        readBin = fromRead . readInt 2 (`elem` "01") (read . pure)

        parseOct :: String -> Parser LispVal
        parseOct = liftNumber . fromRead . readOct

        parseDec :: String -> Parser LispVal
        parseDec s = do
            let parses = reads s
            guard . not $ null parses
            liftNumber . fst $ head parses

        parseHex :: String -> Parser LispVal
        parseHex = liftNumber . fromRead . readHex

        fromRead :: [(Integer, String)] -> Integer
        fromRead [] = 0
        fromRead (x:_) = fst x

        liftNumber :: Integer -> Parser LispVal
        liftNumber = return . Number

parseChar :: Parser LispVal
parseChar = do
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
                          notFollowedBy $ alphaNum <|> symbol
                          return c)
    return $ Char char

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

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

