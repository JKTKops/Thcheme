module Primitives.IOPrimitives (primitives) where

import System.IO (IOMode(..)
                 , openFile, hClose, hGetLine, hPrint
                 , getLine, stdin, stdout)
import Control.Monad.Except (throwError, liftIO)

import LispVal (LispVal(..), LispErr(..), IOThrowsError, IOPrimitive
               , liftThrows)
import Parsers (readExpr, load)

primitives :: [(String, IOPrimitive)]
primitives = [ ("apply", return . head)
             , ("open-input-file", makePort ReadMode)
             , ("open-output-file", makePort WriteMode)
             , ("close-port", closePort)
             , ("read", readProc)
             , ("read-line", readLineProc)
             , ("write", writeProc)
             , ("read-contents", readContents)
             , ("read-all", readAll)
             ]

-- TODO think of a way to uncurry a given Func LispVal
{-applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args-}

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = Port <$> liftIO (openFile filename mode)
makePort _ [badArg]             = throwError $ TypeMismatch "string" badArg
makePort _ badArgs              = throwError $ NumArgs 1 badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []         = readProc [Port stdin]
readProc [Port hdl] = liftIO (hGetLine hdl) >>= liftThrows . readExpr
readProc [badArg]   = throwError $ TypeMismatch "port" badArg
readProc badArgs    = throwError $ NumArgs 1 badArgs

readLineProc :: [LispVal] -> IOThrowsError LispVal
readLineProc []      = String <$> liftIO getLine
readLineProc badArgs = throwError $ NumArgs 0 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]           = writeProc [obj, Port stdout]
writeProc [obj, Port hdl] = liftIO $ hPrint hdl obj >> return (Bool True)
writeProc [obj, badArg]   = throwError $ TypeMismatch "port" badArg
writeProc badArgs         = throwError $ NumArgs 1 badArgs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> liftIO (readFile filename)
readContents [badArg]          = throwError $ TypeMismatch "string" badArg
readContents badArgs           = throwError $ NumArgs 1 badArgs

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename
