{-# LANGUAGE LambdaCase #-}
module Primitives.IOPrimitives (primitives) where

import System.IO ( IOMode (..)
                 , openFile, hClose, hGetLine, hPutStr
                 , getLine, stdin, stdout)

import Val
import EvaluationMonad
import Parsers (readExpr, load)
import Primitives.String (stringSH, unwrapStringPH)
import Primitives.WriteLib (writeSH, writeSimpleSH, writeSharedSH)

primitives :: [Primitive]
primitives = [ openInputFile
             , openOutputFile
             , closePort
             , readP
             , readLineP
             , writeP, writeSimpleP, writeSharedP
             , readContents
             , readAll
             ]

makePort :: String -> IOMode -> Primitive
makePort name mode = Prim name (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          Port <$> liftIO (openFile filename mode)
        | otherwise -> throwError $ TypeMismatch "string" val
  _ -> panic $ "makePort@" ++ name ++ " arity"

openInputFile, openOutputFile :: Primitive
openInputFile  = makePort "open-input-file" ReadMode
openOutputFile = makePort "open-output-file" WriteMode

closePort :: Primitive
closePort = Prim "close-port" (Exactly 1) $ \case
    [Port port] -> liftIO $ hClose port >> return (Bool True)
    _           -> return $ Bool False

readP :: Primitive
readP = Prim "read" (Between 0 1) read
  where
    read :: Builtin
    read []         = read [Port stdin]
    read [Port hdl] = liftIO (hGetLine hdl) >>= liftEither . readExpr
    read [badArg]   = throwError $ TypeMismatch "port" badArg
    read _ = panic "read arity"

readLineP :: Primitive
readLineP = Prim "read-line" (Exactly 0) $ \case
    [] -> liftIO getLine >>= fmap String . newRef
    _ -> panic "readLine arity"

{- TODO: [r7rs]
we need to both define 'display' and fix write, as well as adding
'write-simple'.

Furthermore, we don't define or use 'current-input-port' or any of the
port operations like 'with-output-string' or whatever it's called.
We can/should split these operations into Port.hs and Display.hs or something
to that effect. Expect 'write' to become rather involved due to the requirement
that it use datum labels to show recursive structure.
-}

writeP, writeSimpleP, writeSharedP :: Primitive
[writeP, writeSimpleP, writeSharedP] = map make
  [ ("write", writeSH)
  , ("write-simple", writeSimpleSH)
  , ("write-shared", writeSharedSH)
  ]
  where make (name, writer) = Prim name (Between 1 2) $ mkWriter writer

-- TODO: lmao this is mega broken, need to use writeSH not show
mkWriter :: (Val -> IO String) -> Builtin
mkWriter writer = builtin
  where
    builtin :: Builtin
    builtin [obj]           = builtin [obj, Port stdout]
    builtin [obj, Port hdl] = liftIO $ do
      str <- writer obj
      hPutStr hdl str
      return $ Bool True
    builtin [_obj, badArg]  = throwError $ TypeMismatch "port" badArg
    builtin _ = panic "write arity"

readContents :: Primitive
readContents = Prim "read-contents" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          contents <- liftIO (readFile filename)
          String <$> newRef contents
        | otherwise -> throwError $ TypeMismatch "string" val
  _ -> panic "readContents arity"

readAll :: Primitive
readAll = Prim "read-all" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          load filename >>= makeMutableList
        | otherwise -> throwError $ TypeMismatch "string" val
  _ -> panic "readAll arity"
