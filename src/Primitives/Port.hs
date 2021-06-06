-- | Scheme primitive port operations and Haskell-level
-- operations for working with Ports. Currently simple,
-- but will become more involved in the near future.
--
-- In the style of Haskell's handle operations, functions
-- which take a port have their name prefixed with 'p'.
-- Reading functions return results in a 'Maybe'. 'Nothing'
-- represents an end-of-file, rather than throwing an
-- IOError satisfying isEOFError.
--
-- Haskell-level function assumptions:
--
--   (1) Ports are appropriately input or output ports
--
--   (2) Ports are appropriately textual or binary ports
--
--   (3) Ports are appropriately open (i.e., except to pClose)
--
-- Wrappers should validate these assumptions before calling, with:
--
--   (1) 'pIsReadable' or 'pIsWriteable'
--
--   (2) 'pIsTextual' or 'pIsBinary'
--
--   (3) 'pIsOpen'
--
-- This file is intentionally light on imports of other Thcheme modules,
-- because it implements a fairly complicated but important Thcheme primitive
-- object that we want to be able to freely import where needed. Most of the
-- Scheme-level applications of ports can be found in IOPrimitives.hs.
{-# LANGUAGE ViewPatterns #-}
module Primitives.Port where

import Data.Functor ((<&>))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
    ( hClose,
      hFlush,
      hIsOpen,
      hIsReadable,
      hIsWritable,
      hLookAhead,
      openFile,
      stderr,
      stdin,
      stdout,
      hGetChar,
      hPutChar,
      hReady,
      IOMode(WriteMode, ReadMode) )
import System.IO.Error (catchIOError, isEOFError)

import Val
import EvaluationMonad (panic)

-- | Haskell-level input-port? primitive.
pIsReadable :: Port -> IO Bool
pIsReadable (HandlePort hdl) = hIsReadable hdl
pIsReadable (InputTextPort _ _) = pure True
pIsReadable (OutputTextPort _ _) = pure False

-- | Haskell-level output-port? primitive.
pIsWriteable :: Port -> IO Bool
pIsWriteable (HandlePort hdl) = hIsWritable hdl
pIsWriteable (InputTextPort _ _) = pure False
pIsWriteable (OutputTextPort _ _) = pure True

-- | Haskell-level textual-port? primitive.
pIsTextual :: Port -> IO Bool
pIsTextual _ = pure True -- no binary ports currently

-- | Haskell-level binary-port? primitive.
pIsBinary :: Port -> IO Bool
pIsBinary _ = pure False

pIsOpen :: Port -> IO Bool
pIsOpen (HandlePort hdl) = hIsOpen hdl
pIsOpen (InputTextPort _ isOpen) = readIORef isOpen
pIsOpen (OutputTextPort _ isOpen) = readIORef isOpen

pClose :: Port -> IO ()
pClose (HandlePort hdl) = hClose hdl
pClose (InputTextPort payload isOpen) = do
  writeIORef payload $! "" -- this is not [], it's a Text
  writeIORef isOpen False
pClose (OutputTextPort payload isOpen) = do
  writeIORef payload []
  writeIORef isOpen False

catchEOF :: IO a -> IO (Maybe a)
catchEOF action = catchIOError (Just <$> action) $ \e ->
  if isEOFError e
    then pure Nothing
    else ioError e

pPeekChar :: Port -> IO (Maybe Char)
pPeekChar (HandlePort hdl) = catchEOF $ hLookAhead hdl
pPeekChar (InputTextPort t _) = readIORef t <&> fmap fst . T.uncons
pPeekChar (OutputTextPort _ _) = notReadablePanic "pPeekChar"

pReadChar :: Port -> IO (Maybe Char)
pReadChar (HandlePort hdl) = catchEOF $ hGetChar hdl
pReadChar (InputTextPort t _) = do
  txt <- readIORef t
  case T.uncons txt of
    Nothing -> return Nothing
    Just (c,txt') -> do { writeIORef t $! txt' ; return $ Just c }
pReadChar (OutputTextPort _ _) = notReadablePanic "pReadChar"

pReadLine :: Port -> IO (Maybe Text)
pReadLine (HandlePort hdl) = catchEOF $ TIO.hGetLine hdl
pReadLine (InputTextPort iot _) = do
  txt <- readIORef iot
  let span = T.span notEndOfLine txt
  case span of
    -- empty line, didn't find an eol either, i.e, end of port
    ("", "") -> return Nothing
    -- nonempty line, but didn't find eol, i.e. read to end of port
    (ln, "") -> (writeIORef iot $! "") >> return (Just ln)
    -- any amount of line terminated by an end of line,
    -- process and skip the line ending
    (ln, rest) -> do
      writeIORef iot $! dropOneLineEnding rest
      return $ Just ln
  where
    notEndOfLine '\r' = False
    notEndOfLine '\n' = False
    notEndOfLine _ = True

    dropOneLineEnding (T.uncons -> Just (le, rest0))
      | '\n' <- le = rest0
      | '\r' <- le = case T.uncons rest0 of
        Just ('\n', rest1) -> rest1
        _other -> rest0
    dropOneLineEnding _ = panic "dropOneLineEnding: no line ending"

pReadLine (OutputTextPort _ _) = notReadablePanic "pReadLine"

pCharReady :: Port -> IO Bool
pCharReady (HandlePort hdl) = catchEOF (hReady hdl) <&> \case
  Nothing -> True
  Just b  -> b
pCharReady (InputTextPort iot _) = readIORef iot <&> not . T.null
pCharReady (OutputTextPort _ _) = notReadablePanic "pCharReady"

pReadString :: Int -> Port -> IO (Maybe Text)
pReadString k (HandlePort hdl) = do
  mChar0 <- eofOrChar
  case mChar0 of
    Nothing -> return Nothing
    Just c0 -> untilNothing (replicate (k-1) eofOrChar)
           <&> Just . T.pack . (c0:)
  where
    eofOrChar = catchEOF $ hGetChar hdl
    untilNothing [] = pure []
    untilNothing (m:ms) = do
      mc <- m
      case mc of
        Nothing -> return []
        Just c  -> (c:) <$> untilNothing ms
pReadString k (InputTextPort iot _) = do
  txt <- readIORef iot
  case T.splitAt k txt of
    -- no characters were available, we don't care if
    -- k was zero or not
    ("", "") -> return Nothing
    (str, rest) -> do { writeIORef iot $! rest ; return $ Just str }
pReadString _k (OutputTextPort _ _) = notReadablePanic "pReadString"

-- | Write a character (not it's external representation) to the given
-- textual output port. To file- (or system device-) backed ports,
-- the underlying Haskell handle will perform native newline conversion
-- by default. To string input or output ports, Thcheme will always
-- represent a newline as #\newline, the linefeed character.
pWriteChar :: Char -> Port -> IO ()
pWriteChar c (HandlePort hdl) = hPutChar hdl c
pWriteChar _c (InputTextPort _ _) = notWriteablePanic "pWriteChar"
pWriteChar c (OutputTextPort chunks _) = modifyIORef' chunks (T.singleton c :)

-- | Write the characters of the given Text to the given port, from left
-- to right. Scheme's write-string procedure is supposed to also accept
-- optional index boundaries, but this function is lower level.
pWriteString :: Text -> Port -> IO ()
pWriteString s (HandlePort hdl) = TIO.hPutStr hdl s
pWriteString _s (InputTextPort _ _) = notWriteablePanic "pWriteString"
pWriteString s (OutputTextPort chunks _) = modifyIORef' chunks (s:)

pFlush :: Port -> IO ()
pFlush (HandlePort hdl) = hFlush hdl
pFlush _ = pure ()

-- | Standard IO ports.
pStdin, pStdout, pStderr :: Port
pStdin  = HandlePort stdin
pStdout = HandlePort stdout
pStderr = HandlePort stderr

-- | Create a new input HandlePort backed by the given filename.
pOpenInputFile :: Text -> IO Port
pOpenInputFile fn = HandlePort <$> openFile (unpack fn) ReadMode

-- | Create a new output HandlePort backed by the given filename.
pOpenOutputFile :: Text -> IO Port
pOpenOutputFile fn = HandlePort <$> openFile (unpack fn) WriteMode

-- | Create a new InputTextPort that delivers characters from the given text.
pOpenInputString :: Text -> IO Port
pOpenInputString t = InputTextPort <$> newIORef t <*> newIORef True

-- | Create a new OutputTextPort.
pOpenOutputString :: IO Port
pOpenOutputString = OutputTextPort <$> newIORef [] <*> newIORef True

-- | Freak out because a port isn't readable.
notReadablePanic :: String -> a
notReadablePanic who = panic $ who ++ ": port is not readable"

-- | Freak out because a port isn't writeable.
notWriteablePanic :: String -> a
notWriteablePanic who = panic $ who ++ ": port is not writeable"
