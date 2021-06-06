-- | Scheme primitive Port exports.
--
-- This module exports Scheme primitives from r7rs section 6.13.1.
-- Not all functions there are primitive, in particular, some of
-- them require access to the port parameter objects that aren't
-- available until building the standard library. This module exports
-- enough of them to build the rest in Thcheme itself.
module Primitives.Port (primitives) where

import Control.Monad.Loops (andM)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import System.IO (IOMode(..))
import System.IO.Error

import Types.Port
import Types.Unwrappers
import Val
import EvaluationMonad
import Evaluation (call)

primitives :: [Primitive]
primitives =
  [ callWithPortP
  , callWithInputFileP, callWithOutputFileP
  , isInputPortP, isOutputPortP, isTextualPortP, isBinaryPortP, isPortP
  , isInputPortOpenP, isOutputPortOpenP
  , openInputFileP, openOutputFileP
  , closePortP, closeInputPortP, closeOutputPortP
  , openInputStringP, openOutputStringP, getOutputStringP

  -- non-standard procedures to retrieve standard i/o ports
  -- for use in the standard library definitions of the
  -- current-xxx-port parameter objects.
  , stdinP, stdoutP, stderrP
  ]

callWithPortP :: Primitive
callWithPortP = Prim "call-with-port" (Exactly 2) callWithPortB

callWithPortB :: Builtin
callWithPortB [port, proc] = do
  p <- unwrapPort port
  r <- call proc [port]
  liftIO $ pClose p
  return r
callWithPortB _ = panic "callWithPort arity"

callWithInputFileP, callWithOutputFileP :: Primitive
(callWithInputFileP, callWithOutputFileP)
  = ( mk "call-with-input-file" ReadMode
    , mk "call-with-output-file" WriteMode
    )
  where
    mk name mode = Prim name (Exactly 2) $
      \[str, proc] -> do
        port <- openFileB mode [str]
        callWithPortB [port, proc]

isInputPortP, isOutputPortP, isTextualPortP
  , isBinaryPortP, isPortP :: Primitive
(isInputPortP, isOutputPortP, isTextualPortP, isBinaryPortP, isPortP)
  = ( mk "input-port?" pIsReadable
    , mk "output-port?" pIsWriteable
    , mk "textual-port?" pIsTextual
    , mk "binary-port?" pIsBinary
    , mk "port?" $ const (pure True)
    )
  where
    mk name test = Prim name (Exactly 1) $ \case
      [Port port] -> liftIO $ Bool <$> test port
      [_notPort]  -> return $ Bool False
      _           -> panic $ unpack name ++ " arity"

-- I'm interpreting r7rs here to mean that the argument
-- must be a port, and e.g. input-port-open? returns #f
-- on output ports instead of signaling an error.
isInputPortOpenP, isOutputPortOpenP :: Primitive
(isInputPortOpenP, isOutputPortOpenP)
  = ( mk "input-port-open?" pIsReadable
    , mk "output-port-open?" pIsWriteable
    )
  where
    mk name test = Prim name (Exactly 1) $
      \[v] -> do
        p <- unwrapPort v
        b <- liftIO $ andM [pIsOpen p, test p]
        return $ Bool b

makeFilePort :: Symbol -> IOMode -> Primitive
makeFilePort name mode = Prim name (Exactly 1) $ openFileB mode

openFileB :: IOMode -> Builtin
openFileB mode [val] = do
  filename <- unwrapStr val
  eRes <- liftIO $ tryIOError (Port <$> fnToPort filename)
  case eRes of
    Right port -> return port
    -- TODO: should throw something that satisfies 'file-error?'
    Left ioe -> throwError $ Default $ show ioe
  where
    fnToPort = case mode of
      ReadMode -> pOpenInputFile
      WriteMode -> pOpenOutputFile
      ReadWriteMode -> panic "Thcheme does not support 'rw' file ports"
      AppendMode    -> panic "Thcheme does not support 'a' file ports"
openFileB _ _ = panic "openFile arity"

openInputFileP, openOutputFileP :: Primitive
openInputFileP  = makeFilePort "open-input-file" ReadMode
openOutputFileP = makeFilePort "open-output-file" WriteMode

-- | Given the primitive name and an unwrapper (which should
-- assert the type of the Port as well as unwrapping it),
-- construct a primitive that closes a given Port.
makePortCloser :: Symbol -> (Val -> EM Port) -> Primitive
makePortCloser name unwrap = Prim name (Exactly 1) $
  \[val] -> do
    p <- unwrap val
    liftIO $ pClose p
    return $ MultipleValues []

closePortP, closeInputPortP, closeOutputPortP :: Primitive
closePortP = makePortCloser "close-port" unwrapPort
closeInputPortP = makePortCloser "close-input-port" unwrapInputPort
closeOutputPortP = makePortCloser "close-output-port" unwrapOutputPort

openInputStringP :: Primitive
openInputStringP = Prim "open-input-string" (Exactly 1) $
  \[v] -> do
    str <- unwrapStr v
    liftIO $ Port <$> pOpenInputString str

openOutputStringP :: Primitive
openOutputStringP = Prim "open-output-string" (Exactly 0) $
  \[] -> liftIO $ Port <$> pOpenOutputString

getOutputStringP :: Primitive
getOutputStringP = Prim "get-output-string" (Exactly 1) $
  \[v] -> do
    port <- unwrapOpenPort v
    case port of
      OutputTextPort payloadRef _ -> liftIO $ do
        chunks <- readIORef payloadRef
        let collected = T.concat $ reverse chunks
        writeIORef payloadRef [collected]
        String <$> newIORef collected
      _ -> throwError $ TypeMismatch "output string port" v

-- Some constant functions which just yield the standard
-- communication ports. The standard library won't export
-- these, rather, it will use them to create the initial
-- values of the current-xxx-port parameter objects.
stdinP, stdoutP, stderrP :: Primitive
(stdinP, stdoutP, stderrP)
  = ( mk "standard-input-port"  pStdin
    , mk "standard-output-port" pStdout
    , mk "standard-error-port"  pStderr
    )
  where
    mk name port = Prim name (Exactly 0) $
      \[] -> return $ Port port
