{-# LANGUAGE TemplateHaskell #-}
module Bootstrap (
      nullEnv
    , primitiveBindings
    ) where

import Data.IORef
import Data.String
import Data.FileEmbed
import qualified Data.HashMap.Strict as Map
import Text.ParserCombinators.Parsec

import Primitives (primitives)
import Environment (Env, bindVars)

import Evaluation (evaluate)

nullEnv :: IO Env
nullEnv = newIORef Map.empty

primitiveBindings :: IO Env
primitiveBindings = do
    ne <- nullEnv
    env <- bindVars ne primitives
    let (Right exprs) = parse parseExprs "stdlib" stdlib
    mapM_ (evaluate env Map.empty) exprs
    putStrLn "loaded: stdlib.thm"
    return env

stdlib :: String
stdlib = $(embedStringFile "src/stdlib.thm")

parseExprs :: Parser [String]
parseExprs = try lineComment >>
             endBy parseExpr (skipMany $ (:[]) <$> space
                                     <|> lineComment
                                     <|> try blockComment)

parseExpr :: Parser String
parseExpr = do
    open <- oneOf "([{"
    internal <- fmap concat . many $
            try blockComment
        <|> lineComment
        <|> ((:[]) <$> noneOf "([{}])")
        <|> parseExpr
    close <- oneOf ")]}"
    return $ [open] ++ internal ++ [close]

blockComment :: Parser String
blockComment = do
    string "#|"
    manyTill anyChar . try $ string "|#"
    return ""

lineComment :: Parser String
lineComment = do
    char ';'
    manyTill anyChar $ char '\n'
    return ""
