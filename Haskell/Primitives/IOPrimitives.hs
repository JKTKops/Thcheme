module Primitives.IOPrimitives (primitives) where

import System.IO (IOMode(..)
                 , openFile, hClose, hGetLine, hPrint
                 , getLine, stdin, stdout)
import Control.Monad.Except (throwError, liftIO)

import LispVal (LispVal(..), LispErr(..), IOThrowsError, IOPrimitive
               , liftThrows)
import Parsers (readExpr, load)
import Evaluation (apply)

primitives :: [(String, IOPrimitive)]
primitives = [ ("apply", applyProc)
             , ("open-input-file", makePort ReadMode)
             , ("open-output-file", makePort WriteMode)
             , ("close-port", closePort)
             , ("read", readProc)
             , ("read-line", readLineProc)
             , ("write", writeProc)
             , ("read-contents", readContents)
             , ("read-all", readAll)
             ]

applyProc :: IOPrimitive
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> IOPrimitive
makePort mode [String filename] = Port <$> liftIO (openFile filename mode)
makePort _ [badArg]             = throwError $ TypeMismatch "string" badArg
makePort _ badArgs              = throwError $ NumArgs 1 badArgs

closePort :: IOPrimitive
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: IOPrimitive
readProc []         = readProc [Port stdin]
readProc [Port hdl] = liftIO (hGetLine hdl) >>= liftThrows . readExpr
readProc [badArg]   = throwError $ TypeMismatch "port" badArg
readProc badArgs    = throwError $ NumArgs 1 badArgs

readLineProc :: IOPrimitive
readLineProc []      = String <$> liftIO getLine
readLineProc badArgs = throwError $ NumArgs 0 badArgs

writeProc :: IOPrimitive
writeProc [obj]           = writeProc [obj, Port stdout]
writeProc [obj, Port hdl] = liftIO $ hPrint hdl obj >> return (Bool True)
writeProc [obj, badArg]   = throwError $ TypeMismatch "port" badArg
writeProc badArgs         = throwError $ NumArgs 1 badArgs

readContents :: IOPrimitive
readContents [String filename] = String <$> liftIO (readFile filename)
readContents [badArg]          = throwError $ TypeMismatch "string" badArg
readContents badArgs           = throwError $ NumArgs 1 badArgs

readAll :: IOPrimitive
readAll [String filename] = List <$> load filename
