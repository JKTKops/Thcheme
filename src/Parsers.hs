{-# LANGUAGE BlockArguments #-}
module Parsers
    ( readExpr
    , labeledReadExpr
    , readExprList
    , labeledReadExprList
    , load
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except (MonadError, MonadIO, liftIO)

import Parsers.Internal
import Val
import EvaluationMonad (liftEither)

labeledReadExpr :: String -> String -> Either LispErr Val
labeledReadExpr label = liftEither . labeledReadOrThrow label do
    whiteSpace
    expr <- parseExpr
    eof
    return expr

readExpr :: String -> Either LispErr Val
readExpr = labeledReadExpr "Thcheme"

labeledReadExprList :: String -> String -> Either LispErr [Val]
labeledReadExprList label = labeledReadOrThrow label do
    whiteSpace
    exprs <- many parseExpr
    eof
    return exprs

readExprList :: String -> Either LispErr [Val]
readExprList = labeledReadExprList "Thcheme"

load :: (MonadIO m, MonadError LispErr m) => String -> m [Val]
load filename = liftIO (readFile filename) >>= liftEither . readExprList
