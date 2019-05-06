module Parsers (readExpr, readExprList, load) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except (throwError, liftIO)

import Parsers.Internal
import Types

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow $ do
    expr <- parseExpr
    eof
    return expr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
