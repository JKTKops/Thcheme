module Types.Unwrappers where

import Control.Monad (unless)

import Val ( Val (..), Symbol, Number (..), RealNumber (..), Port (..)
           , LispErr(TypeMismatch), Text, getExactInteger )
import EvaluationMonad (EM, liftIO, throwError, readRef)
import Types.Port

unwrapNum :: Val -> EM Number
unwrapNum (Number n) = return n
unwrapNum notNum     = throwError $ TypeMismatch "number" notNum

unwrapRealNum :: Val -> EM RealNumber
unwrapRealNum (Number (Real r)) = pure r
unwrapRealNum notRealNum = throwError $ TypeMismatch "real number" notRealNum

unwrapExactInteger :: Val -> EM Integer
unwrapExactInteger v = case getExactInteger v of
  Just i -> return i
  Nothing -> throwError $ TypeMismatch "exact integer" v

unwrapSymbol :: Val -> EM Symbol
unwrapSymbol (Symbol s) = pure s
unwrapSymbol notSymbol = throwError $ TypeMismatch "symbol" notSymbol

unwrapStr :: Val -> EM Text
unwrapStr (IString s) = return s
unwrapStr (String ref) = readRef ref
unwrapStr notString  = throwError $ TypeMismatch "string" notString

unwrapChar :: Val -> EM Char
unwrapChar (Char c) = return c
unwrapChar notChar  = throwError $ TypeMismatch "char" notChar

unwrapBool :: Val -> EM Bool
unwrapBool (Bool b) = return b
unwrapBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- more involved unwrappers can be found in IOPrimitives.
unwrapPort :: Val -> EM Port
unwrapPort (Port p) = return p
unwrapPort notPort  = throwError $ TypeMismatch "port" notPort

-------------------------------------------------------------------------------
-- Port unwrappers that validate ports have certain types
-------------------------------------------------------------------------------

ensureInputPort :: Port -> EM ()
ensureInputPort p = unlessM (liftIO $ pIsReadable p) $ throwError notInput
  where notInput = TypeMismatch "input port" $ Port p

ensureOutputPort :: Port -> EM ()
ensureOutputPort p = unlessM (liftIO $ pIsWriteable p) $ throwError notOutput
  where notOutput = TypeMismatch "output port" $ Port p

ensureTextualPort :: Port -> EM ()
ensureTextualPort p = unlessM (liftIO $ pIsTextual p) $ throwError notTextual
  where notTextual = TypeMismatch "textual port" $ Port p

ensureBinaryPort :: Port -> EM ()
ensureBinaryPort p = unlessM (liftIO $ pIsBinary p) $ throwError notBinary
  where notBinary = TypeMismatch "binary port" $ Port p

ensureOpenPort :: Port -> EM ()
ensureOpenPort p = unlessM (liftIO $ pIsOpen p) $ throwError notOpen
  where notOpen = TypeMismatch "open port" $ Port p

unwrapPortWithEnsures :: [Port -> EM ()] -> Val -> EM Port
unwrapPortWithEnsures ensures = unwrap
  where
    applyEnsures p = mapM_ ($ p) ensures
    unwrap v = do
      p <- unwrapPort v
      applyEnsures p
      return p

unwrapOpenTextualInputPort :: Val -> EM Port
unwrapOpenTextualInputPort = unwrapPortWithEnsures
  [ ensureOpenPort
  , ensureInputPort
  , ensureTextualPort
  ]

unwrapOpenTextualOutputPort :: Val -> EM Port
unwrapOpenTextualOutputPort = unwrapPortWithEnsures
  [ ensureOpenPort
  , ensureOutputPort
  , ensureTextualPort
  ]

unwrapOpenBinaryInputPort :: Val -> EM Port
unwrapOpenBinaryInputPort = unwrapPortWithEnsures
  [ ensureOpenPort
  , ensureInputPort
  , ensureBinaryPort
  ]

unwrapOpenBinaryOutputPort :: Val -> EM Port
unwrapOpenBinaryOutputPort = unwrapPortWithEnsures
  [ ensureOpenPort
  , ensureOutputPort
  , ensureBinaryPort
  ]

-- close-input-port and close-output-port don't ask us to guarantee
-- that the ports are textual/binary or open, just that they are
-- appropriately input/output.

unwrapInputPort :: Val -> EM Port
unwrapInputPort = unwrapPortWithEnsures [ensureInputPort]

unwrapOutputPort :: Val -> EM Port
unwrapOutputPort = unwrapPortWithEnsures [ensureOutputPort]

-- get-output-string can check the port type easily with 'case',
-- (and produce a better error message that way) but still
-- needs to check that the port is open.

unwrapOpenPort :: Val -> EM Port
unwrapOpenPort = unwrapPortWithEnsures [ensureOpenPort]

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test act = test >>= \b -> unless b act
