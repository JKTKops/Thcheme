module Parsers.Internal where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Maybe (isNothing)
import Data.Array (listArray)
import Control.Monad (liftM, guard)
import Control.Monad.Except (throwError, liftIO)
import Numeric (readInt, readOct, readHex)

import LispVal

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "Thcheme" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr = try parseNumber
         <|> try parseChar -- #\atom is a valid name for an atom
         <|> try parseVector
         <|> parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do brace <- char '(' <|> char '[' <|> char '{'
                x <- try parseDottedList <|> parseList
                char $ close brace
                return x
  where close brace = case brace of
            '(' -> ')'
            '[' -> ']'
            '{' -> '}'

symbol :: Parser Char
symbol = oneOf "!@#$%^&*-_=+|:\\/?<>~"

spaces :: Parser ()
spaces = skipMany1 space

delim :: Parser ()
delim = notFollowedBy $ alphaNum <|> symbol

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (fmap pure notEscape <|> escape <|> fmap pure space)
    char '"'
    return $ String . concat $ x
  where notEscape = noneOf "\\\"\n\r\t"
        escape = do
            d <- char '\\'
            c <- oneOf "\\\"nrt" <?> "escape character"
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
    delim
    if isNothing sign
    then return num
    else let Number n = num in
        return . Number $ negate n

  where parseRadixPrefix :: Parser String
        parseRadixPrefix = do
            hash <- char '#'
            prefix <- oneOf "bodh"
            return [hash, prefix]

        parseBin :: String -> Parser LispVal
        parseBin = liftNumber . readBin

        readBin :: String -> Maybe Integer
        readBin = fromRead . readInt 2 (`elem` "01") (read . pure)

        parseOct :: String -> Parser LispVal
        parseOct = liftNumber . fromRead . readOct

        parseDec :: String -> Parser LispVal
        parseDec = liftNumber . fromRead . reads

        parseHex :: String -> Parser LispVal
        parseHex = liftNumber . fromRead . readHex

        fromRead :: [(Integer, String)] -> Maybe Integer
        fromRead [] = Nothing
        fromRead xs = case filter (\(_, s) -> null s) xs of
            (x:_) -> Just $ fst x
            []    -> Nothing

        liftNumber :: Maybe Integer -> Parser LispVal
        liftNumber mx = case mx of
            Just x  -> return $ Number x
            Nothing -> unexpected "number parsing failure"

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
                          delim
                          return c)
    return $ Char char

parseVector :: Parser LispVal
parseVector = do
    string "#("
    exprs <- sepBy parseExpr spaces
    char ')'
    return . Vector . listArray (0, fromIntegral $ length exprs - 1) $ exprs

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr (skipMany space)

parseDottedList :: Parser LispVal
parseDottedList = do
    init <- endBy parseExpr spaces
    last <- char '.' >> spaces >> parseExpr
    case (init, last) of
        ([], _)         -> return last
        (init, List ls) -> return . List $ init ++ ls
        (init, val)     -> return $ DottedList init val

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

