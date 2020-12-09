{-# LANGUAGE LambdaCase #-}
module Primitives.IOPrimitives (primitives) where

import System.IO ( IOMode (..)
                 , openFile, hClose, hGetLine, hPutStr
                 , getLine, stdin, stdout)

import Val
import EvaluationMonad (liftEither, MonadIO (liftIO), MonadError (throwError))
import Parsers (readExpr, load)

primitives :: [Primitive]
primitives = [ openInputFile
             , openOutputFile
             , closePort
             , readProc
             , readLineProc
             , writeProc
             , writeToPort
             , readContents
             , readAll
             ]

makePort :: String -> IOMode -> Primitive
makePort name mode = Prim name 1 $ \case
    [String filename] -> Port <$> liftIO (openFile filename mode)
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs

openInputFile, openOutputFile :: Primitive
openInputFile  = makePort "open-input-file" ReadMode
openOutputFile = makePort "open-output-file" WriteMode

closePort :: Primitive
closePort = Prim "close-port" 1 $ \case
    [Port port] -> liftIO $ hClose port >> return (Bool True)
    _           -> return $ Bool False

readProc :: Primitive
readProc = Prim "read" 0 read
  where
    read :: Builtin
    read []         = read [Port stdin]
    read [Port hdl] = liftIO (hGetLine hdl) >>= liftEither . readExpr
    read [badArg]   = throwError $ TypeMismatch "port" badArg
    read badArgs    = throwError $ NumArgs 1 badArgs

readLineProc :: Primitive
readLineProc = Prim "read-line" 0 $ \case
    []      -> String <$> liftIO getLine
    badArgs -> throwError $ NumArgs 0 badArgs

{- TODO: [r7rs]
we need to both define 'display' and fix write, as well as adding
'write-simple'.

Furthermore, we don't define or use 'current-input-port' or any of the
port operations like 'with-output-string' or whatever it's called.
We can/should split these operations into Port.hs and Display.hs or something
to that effect. Expect 'write' to become rather involved due to the requirement
that it use datum labels to show recursive structure.
-}

writeProc :: Primitive
writeProc = Prim "write" 1 write

writeToPort :: Primitive
writeToPort = Prim "write-port" 2 write

write :: Builtin
write [obj]           = write [obj, Port stdout]
write [obj, Port hdl] = liftIO $ hPutStr hdl (show obj) >> return (Bool True)
write [obj, badArg]   = throwError $ TypeMismatch "port" badArg
write badArgs         = throwError $ NumArgs 1 badArgs

readContents :: Primitive
readContents = Prim "read-contents" 1 $ \case
    [String filename] -> String <$> liftIO (readFile filename)
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs

readAll :: Primitive
readAll = Prim "read-all" 1 $ \case
    [String filename] -> load filename >>= makeMutableList
    [badArg]          -> throwError $ TypeMismatch "string" badArg
    badArgs           -> throwError $ NumArgs 1 badArgs
