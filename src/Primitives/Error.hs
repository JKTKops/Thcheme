module Primitives.Error
  ( primitives
  ) where

import Val
import Types (ExceptionCont(..))
import EvaluationMonad
  ( EM, ExceptionHandler, currentHandlers
  , callCC, panic, throwError, gets
  , writeRef, readRef, modifyRef'
  )
import Evaluation
import Primitives.Misc (dynamicWindB, valuesB)
import Types.Unwrappers (unwrapStr)

primitives :: [Primitive]
primitives =
  [ errorObjectP, isErrorObjectP, errorObjectMessageP, errorObjectIrritantsP
  , isFileErrorP, isReadErrorP
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

-- Note: [file-error? and read-error?]
-- There's no requirements according to r7rs that file-error? and read-error?
-- be disjoint from anything else. For ease of application, therefore, we
-- implement file errors as regular Error objects with the message "file error"
-- and relevant but arbitrary irritants (probably the filename).
-- Read errors are Exceptions from the Parser.
isFileErrorP :: Primitive
isFileErrorP = Prim "file-error?" (Exactly 1) $ \case
  [Error "file error" _] -> pure $ Bool True
  [_]                    -> pure $ Bool False
  _ -> panic "isFileError arity"

isReadErrorP :: Primitive
isReadErrorP = Prim "read-error?" (Exactly 1) $ \case
  [Exception Parser{}] -> pure $ Bool True
  [_]                  -> pure $ Bool False
  _ -> panic "isReadError arity"

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
  withExceptionHandler (makeExceptionHandler handler) thunk
withExceptionHandlerB _ = panic "withExceptionHandlerB arity"

-- | Low-level version of (scheme base) with-exception-handler.
withExceptionHandler :: ExceptionHandler -- ^ New exception handler to install
                     -> Val -- ^ Scheme procedure accepting 0 arguments to
                            -- call with the new handler installed
                     -> EM Val
withExceptionHandler handler thunk = do
  handlers <- gets currentHandlers >>= readRef
  let newHandlers = handler : handlers
  dynamicWindB [ makeExceptionHandlersSetter newHandlers
               , thunk
               , makeExceptionHandlersSetter handlers
               ]

makeExceptionHandlersSetter :: [ExceptionHandler] -> Val
makeExceptionHandlersSetter handlers =
  Primitive (Exactly 0) setter "with-exception-handler:dynamic-handlers-set!"
  where
    setter _ = do
      handlersRef <- gets currentHandlers
      writeRef handlersRef handlers
      valuesB []

-- Note: [Continuations in conditions]
-- Notice that the continuations in conditions are not continuation objects,
-- merely haskell continuations. The biggest difference between the two is
-- that haskell continuations don't capture a dynamic point. That's no problem
-- for the primitive exception handling facility, because once a continuable 
-- exception is thrown, one of three things happens:
--
--   (1) The program terminates with a 'LispErr' and returns to the prompt, or
--
--   (2) A handler escapes to a (call/cc) captured continuation, which does
--       have a captured dynamic point, and the appropriate reroots will happen
--       at that point.
--
--   (3) The handler returns, in which case this must be the innermost handler
--       in the dynamic extent of the raise-continuable call
--
-- Until one of those two things happens, 'emThrow' will be continually
-- applying the next exception handler and unwinding to the next one if
-- it returns. If it didn't return, it called an escape continuation. Unless
-- a user is going out of their way to make bad things happen, this
-- continuation was captured outside of the with-exception-handler call that
-- raised the error. Rerooting to this continuation's DynamicPoint will
-- reinstall the handlers that were in place in the dynamic extent of that
-- with-exception-handler call. See the reference implementation to SRFI-34,
-- but note that it does not include raise-continuable.

-- | Convert a 'Val' representing a procedure that accepts one argument
-- into an 'ExceptionHandler'.
makeExceptionHandler :: Val -> ExceptionHandler
makeExceptionHandler proc = handler
  where
    handler (Condition Nothing obj) = call proc [obj]
    handler (Condition (Just (EC k)) obj) = do
      r <- call proc [obj]
      handlersRef <- gets currentHandlers
      modifyRef' handlersRef (handler:)
      k r
    handler e = call proc [Exception e]
