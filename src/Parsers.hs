{-# LANGUAGE BlockArguments #-}
module Parsers
    ( readExpr
    , labeledReadExpr
    , readExprList
    , labeledReadExprList
    , load
    ) where

import Control.Monad.Except (MonadError, MonadIO, liftIO)

import Parsers.Parser
import Val
import EvaluationMonad (liftEither)

labeledReadExpr :: String -> String -> Either LispErr Val
labeledReadExpr src inp = mapLeft Parser $ parseDatum src inp

readExpr :: String -> Either LispErr Val
readExpr = labeledReadExpr "thcheme"

labeledReadExprList :: String -> String -> Either LispErr [Val]
labeledReadExprList src inp = mapLeft Parser $ parseDatumSeq src inp

readExprList :: String -> Either LispErr [Val]
readExprList = labeledReadExprList "thcheme"

load :: (MonadIO m, MonadError LispErr m) => String -> m [Val]
load filename = liftIO (readFile filename) >>= liftEither . readExprList

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right a) = Right a