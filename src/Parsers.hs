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
import Types

labeledReadExpr :: String -> String -> ThrowsError LispVal
labeledReadExpr label = labeledReadOrThrow label $ do
    whiteSpace
    expr <- parseExpr
    eof
    return expr

readExpr :: String -> ThrowsError LispVal
readExpr = labeledReadExpr "Thcheme"

labeledReadExprList :: String -> String -> ThrowsError [LispVal]
labeledReadExprList label = labeledReadOrThrow label $ do
    whiteSpace
    exprs <- many parseExpr
    eof
    return exprs

readExprList = labeledReadExprList "Thcheme"

load :: (MonadIO m, MonadError LispErr m) => String -> m [LispVal]
load filename = liftIO (readFile filename) >>= liftEither . readExprList
