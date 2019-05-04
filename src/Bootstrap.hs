{-# LANGUAGE TemplateHaskell #-}
module Bootstrap (
      nullEnv
    , primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
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
    let (Right exprs) = stdlib
    mapM_ (evaluate env Map.empty) exprs
    return env

stdlib :: Either ParseError [String]
stdlib = parse parseExprs "stdlib" $(embedStringFile "src/stdlib.thm")

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
