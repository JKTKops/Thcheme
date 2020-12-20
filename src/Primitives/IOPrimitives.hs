{-# LANGUAGE LambdaCase #-}
module Primitives.IOPrimitives (primitives) where

import System.IO ( IOMode (..)
                 , openFile, hClose, hGetLine, hPutStr
                 , getLine, stdin, stdout)

import Val
import EvaluationMonad
import Parsers (readExpr, load)
import Primitives.String (stringSH, unwrapStringPH)

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
makePort name mode = Prim name (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          Port <$> liftIO (openFile filename mode)
        | otherwise -> throwError $ TypeMismatch "string" val

openInputFile, openOutputFile :: Primitive
openInputFile  = makePort "open-input-file" ReadMode
openOutputFile = makePort "open-output-file" WriteMode

closePort :: Primitive
closePort = Prim "close-port" (Exactly 1) $ \case
    [Port port] -> liftIO $ hClose port >> return (Bool True)
    _           -> return $ Bool False

readProc :: Primitive
readProc = Prim "read" (Between 0 1) read
  where
    read :: Builtin
    read []         = read [Port stdin]
    read [Port hdl] = liftIO (hGetLine hdl) >>= liftEither . readExpr
    read [badArg]   = throwError $ TypeMismatch "port" badArg

readLineProc :: Primitive
readLineProc = Prim "read-line" (Exactly 0) $ \case
    []      -> liftIO getLine >>= fmap String . newRef

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
writeProc = Prim "write" (Between 1 2) write

writeToPort :: Primitive
writeToPort = Prim "write-port" (Exactly 2) write

-- TODO: lmao this is mega broken, need to use writeSH not show
write :: Builtin
write [obj]           = write [obj, Port stdout]
write [obj, Port hdl] = liftIO $ hPutStr hdl (show obj) >> return (Bool True)
write [obj, badArg]   = throwError $ TypeMismatch "port" badArg

readContents :: Primitive
readContents = Prim "read-contents" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          contents <- liftIO (readFile filename)
          String <$> newRef contents
        | otherwise -> throwError $ TypeMismatch "string" val

readAll :: Primitive
readAll = Prim "read-all" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          load filename >>= makeMutableList
        | otherwise -> throwError $ TypeMismatch "string" val
