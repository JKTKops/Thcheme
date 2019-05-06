{-# LANGUAGE LambdaCase #-}
module Primitives.IOPrimitives (ioPrimitives) where

import System.IO ( IOMode (..)
                 , openFile, hClose, hGetLine, hPrint
                 , getLine, stdin, stdout)
import Control.Monad.Except (throwError, liftIO)

import Types
import Parsers (readExpr, load)

ioPrimitives :: [(String, IOPrimitive)]
ioPrimitives = [ ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-port", closePort)
               , ("read", readProc)
               , ("read-line", readLineProc)
               , ("write", writeProc)
               , ("write-port", writeToPort)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]

makePort :: IOMode -> IOPrimitive
makePort mode = IPrim 1 $ \case
    [String filename] -> Port <$> liftIO (openFile filename mode)
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs

closePort :: IOPrimitive
closePort = IPrim 1 $ \case
    [Port port] -> liftIO $ hClose port >> return (Bool True)
    _           -> return $ Bool False

readProc :: IOPrimitive
readProc = IPrim 0 read
  where
    read :: IBuiltin
    read []         = read [Port stdin]
    read [Port hdl] = liftIO (hGetLine hdl) >>= liftThrows . readExpr
    read [badArg]   = throwError $ TypeMismatch "port" badArg
    read badArgs    = throwError $ NumArgs 1 badArgs

readLineProc :: IOPrimitive
readLineProc = IPrim 0 $ \case
    []      -> String <$> liftIO getLine
    badArgs -> throwError $ NumArgs 0 badArgs

writeProc :: IOPrimitive
writeProc = IPrim 1 write

writeToPort :: IOPrimitive
writeToPort = IPrim 2 write

write :: IBuiltin
write [obj]           = write [obj, Port stdout]
write [obj, Port hdl] = liftIO $ hPrint hdl obj >> return (Bool True)
write [obj, badArg]   = throwError $ TypeMismatch "port" badArg
write badArgs         = throwError $ NumArgs 1 badArgs

readContents :: IOPrimitive
readContents = IPrim 1 $ \case
    [String filename] -> String <$> liftIO (readFile filename)
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs

readAll :: IOPrimitive
readAll = IPrim 1 $ \case
    [String filename] -> List <$> load filename
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs
