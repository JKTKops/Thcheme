{-# LANGUAGE BlockArguments #-}
module Parsers
    ( readExpr
    , labeledReadExpr, labeledReadExprFromPort
    , readExprList
    , labeledReadExprList
    , load
    ) where

import Control.Monad.Except (MonadError, MonadIO, liftIO)

import Types.Port (pOpenInputFile, pOpenInputString)
import Parser.Parser (parseDatum, parseDatumSeq)
import EvaluationMonad (liftEither)
import Val

labeledReadExprFromPort :: String -> Port -> Either LispErr Val
labeledReadExprFromPort lbl port =
  mapLeft Parser $ parseDatum lbl port

labeledReadExpr :: String -> String -> IO (Either LispErr Val)
labeledReadExpr lbl inp = do
  port <- pOpenInputString $ pack inp
  return $ mapLeft Parser $ parseDatum lbl port

readExpr :: String -> IO (Either LispErr Val)
readExpr = labeledReadExpr "thcheme"

labeledReadExprList :: String -> Port -> Either LispErr [Val]
labeledReadExprList src port = mapLeft Parser $ parseDatumSeq src port

readExprList :: Port -> Either LispErr [Val]
readExprList = labeledReadExprList "thcheme"

load :: (MonadIO m, MonadError LispErr m) => String -> m [Val]
load filename = liftIO (pOpenInputFile $ pack filename) >>= liftEither . readExprList

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right a) = Right a