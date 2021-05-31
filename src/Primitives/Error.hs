module Primitives.Error
  ( primitives
  ) where

import Val
import Types (ExceptionCont(..), withExceptionHandler)
import EvaluationMonad (EM, callCC, panic, throwError)
import Evaluation
import Primitives.Unwrappers (unwrapStr)

primitives :: [Primitive]
primitives =
  [ errorObjectP, isErrorObjectP, errorObjectMessageP, errorObjectIrritantsP
  , errorP, typeErrorP
  , raiseP
  , raiseContinuableP
  , withExceptionHandlerP
  ]

errorObjectP :: Primitive
errorObjectP = Prim "error-object" (AtLeast 1) errorObjectB

errorObjectB :: [Val] -> EM Val
errorObjectB (msg : irritants) = do
  msgS <- unwrapStr msg
  return $ Error msgS irritants
errorObjectB [] = panic "error-object arity"

isErrorObjectP :: Primitive
isErrorObjectP = Prim "error-object?" (Exactly 1) $ \case
  [Error{}] -> pure $ Bool True
  [_]       -> pure $ Bool False
  _ -> panic "isErrorObject arity"

-- TODO:
-- I think for reasonable semantics, Error objects should really
-- hold the actual object that they were passed.
-- I.e., check if string, but don't unwrap.
-- This will happen naturally if error objects become records.
errorObjectMessageP :: Primitive
errorObjectMessageP = Prim "error-object-message" (Exactly 1) $ \case
  [Error msg _] -> pure $ IString msg
  [notErr] -> throwError $ TypeMismatch "error object" notErr
  _ -> panic "errorObjectMessage arity"

errorObjectIrritantsP :: Primitive
errorObjectIrritantsP = Prim "error-object-irritants" (Exactly 1) $ \case
  [Error _ irrs] -> makeMutableList irrs
  [notErr] -> throwError $ TypeMismatch "error object" notErr
  _ -> panic "errorObjectIrritants arity"

typeErrorP :: Primitive
typeErrorP = Prim "type-error" (Exactly 2) $
  \[tyname, irritant] -> do
    name <- unwrapStr tyname
    throwError $ TypeMismatch (unpack name) irritant

errorP :: Primitive
errorP = Prim "error" (AtLeast 1) $ \vs -> do
  eobj <- errorObjectB vs
  raiseB [eobj]

raiseP :: Primitive
raiseP = Prim "raise" (Exactly 1) raiseB

raiseB :: Builtin
raiseB [x] = throwError $ Condition Nothing x
raiseB _ = panic "raise arity"

raiseContinuableP :: Primitive
raiseContinuableP = Prim "raise-continuable" (Exactly 1) $
  \[x] -> callCC $ \k ->
    throwError $ Condition (Just (EC k)) x

withExceptionHandlerP :: Primitive
withExceptionHandlerP = 
  Prim "with-exception-handler"
       (Exactly 2)
       withExceptionHandlerB

withExceptionHandlerB :: [Val] -> EM Val
withExceptionHandlerB [handler, thunk] =
  withExceptionHandler applyHandler (call thunk [])
  where
    applyHandler v = call handler [v]
withExceptionHandlerB _ = panic "withExceptionHandlerB arity"
