module Parsers
    ( readExpr
    , labeledReadExpr
    , readExprList
    , labeledReadExprList
    , load
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Text.Parsec.Token as Tok
import Control.Monad.Except (throwError, liftIO)

import Parsers.Internal
import Types

labeledReadExpr :: String -> String -> ThrowsError LispVal
labeledReadExpr label = labeledReadOrThrow label $ do
    whiteSpace
    expr <- parseExpr
    eof
    return expr

readExpr = labeledReadExpr "Thcheme"

labeledReadExprList :: String -> String -> ThrowsError [LispVal]
labeledReadExprList label = labeledReadOrThrow label $ do
    whiteSpace
    exprs <- many parseExpr
    eof
    return exprs

readExprList = labeledReadExprList "Thcheme"

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList
