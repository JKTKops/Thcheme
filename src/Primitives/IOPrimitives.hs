{-# LANGUAGE LambdaCase #-}
-- TODO: rename to just IO
module Primitives.IOPrimitives (primitives) where

import Control.Monad (unless)

import Val
import EvaluationMonad
import Parsers (load, labeledReadExprFromPort)
import Primitives.Port
import Primitives.Unwrappers (unwrapStr, unwrapPort)
import Primitives.WriteLib (writeSH, writeSimpleSH, writeSharedSH, displaySH)

primitives :: [Primitive]
primitives = [ openInputFile
             , openOutputFile
             , closePort, closeInputPort, closeOutputPort
             , readP
             , readLineP
             , writeP, writeSimpleP, writeSharedP, displayP
             , newlineP
             , readContents
             , readAll
             ]

makeFilePort :: Symbol -> (Text -> IO Port) -> Primitive
makeFilePort name fnToPort = Prim name (Exactly 1) $ \case
  [val] -> do
    filename <- unwrapStr val
    liftIO $ catchError (Port <$> fnToPort filename) $ \_ioe ->
      return (Bool False) -- TODO: should be an object satisfying
                          -- file-error?
  _ -> panic $ "makePort@" ++ symbolAsString name ++ " arity"

openInputFile, openOutputFile :: Primitive
openInputFile  = makeFilePort "open-input-file" pOpenInputFile
openOutputFile = makeFilePort "open-output-file" pOpenOutputFile

-- | Given the primitive name and an unwrapper (which should
-- assert the type of the Port as well as unwrapping it),
-- construct a primitive that closes a given Port.
makePortCloser :: Symbol -> (Val -> EM Port) -> Primitive
makePortCloser name unwrap = Prim name (Exactly 1) $
  \[val] -> do
    p <- unwrap val
    liftIO $ pClose p
    return $ MultipleValues []

closePort, closeInputPort, closeOutputPort :: Primitive
closePort = makePortCloser "close-port" unwrapPort
closeInputPort = makePortCloser "close-input-port" unwrapInputPort
closeOutputPort = makePortCloser "close-output-port" unwrapOutputPort

readP :: Primitive
readP = Prim "read" (Between 0 1) read
  where
    read :: Builtin
    read []  = --read [Port pStdin]
               throwError $ Default "currently, attempting to (read) stdin will hang"
    read [v] = do
      p <- unwrapOpenTextualInputPort v
      let ev = labeledReadExprFromPort "read" p
      -- TODO: correct the errors (or maybe change 'alexError' to be more
      -- informative?) specifically:
      --   1) EOF but read no characters: return #<eof>
      --   2) any other lexing/parsing error: read-error
      liftEither ev
    read _ = panic "read arity"

readLineP :: Primitive
readLineP = Prim "read-line" (Between 0 1) readLineB

readLineB :: Builtin
readLineB [] = readLineB [Port pStdin]
readLineB [v] = do
  p <- unwrapOpenTextualInputPort v
  mln <- liftIO (pReadLine p)
  case mln of
    Nothing -> return $ Bool False -- TODO: eof-object
    Just ln -> String <$> newRef ln
readLineB _ = panic "readLine arity"

{- TODO: [r7rs]
We don't define or use 'current-input-port' or any of the
port operations like 'with-output-string' or whatever it's called.
We can/should split these operations into Port.hs and Display.hs or something
to that effect.

My thoughts at the moment are that we should prepend 'thcheme:' to the starts
of these primitives and make them _require_ a port argument.
Then in the standard library file we provide these libraries:
(library (thcheme parameters)
  (export make-parameter parameterize)
  (import (for (core syntax-rules)   expand)
          (for (core primitives) run expand)
          (core let)
          (for (core derived)    run expand)
          (primitives dynamic-wind id eq? error
                      list pair? null? cons car cadr))
  
  (define <param-set!> (list '<param-set!>))
  (define <param-convert> (list '<param-convert>))

  (define (make-parameter init . o)
    (let* ([converter
            (if (pair? o) (car o) id)]
           [value (converter init)])
      (lambda args
        (cond
         [(null? args) value]
         [(eq? (car args) <param-set!>)
          (set! value (cadr args))]
         [(eq? (car args) <param-convert>)
          converter]
         [else
          (error "bad parameter syntax: " (cons '<parameter> args))]))))
  
  (define-syntax parameterize
    (syntax-rules ()
      ((parameterize "step"
                     ((param value p old new) ...)
                     ()
                     body)
       (let ([p param] ...)
         (let ([old (p)] ...
               [new ((p <param-convert>) value)] ...)
           (dynamic-wind
            (lambda () (p <param-set!> new) ...)
            (lambda () . body)
            (lambda () (p <param-set!> old) ...)))))
      ((parameterize "step"
                     args
                     ((param value) . rest)
                     body)
       (parameterize "step"
                     ((param value p old new) . args)
                     rest
                     body))
      ((parameterize ((param value) ...) . body)
       (parameterize "step" 
                     ()
                     ((param value) ...)
                     body))))
  ) ; thcheme parameters

(library (thcheme ports)
  (export current-input-port current-output-port current-error-port
          ; and so on
          )
  (import (primitives stdin stdout stderr)
          (thcheme parameters)
          ; and so on
          )
  (define current-input-port  (make-parameter stdin))
  (define current-output-port (make-parameter stdout))
  (define current-error-port  (make-parameter stderr))
  ) ; thcheme ports

(library (scheme write)
  (export write write-simple write-shared display)
  (import (thcheme ports)
          (primitives thcheme:write thcheme:write-simple
                      thcheme:write-shared thcheme:display))
  
  (define (with-default-port proc)
    (lambda (obj . port)
      (let ([port
             (cond [(null? port) (current-input-port)]
                   [(pair? port) (car port)])])
        (proc obj port))))
  (define write        (with-default-port thcheme:write))
  (define write-simple (with-default-port thcheme:write-simple))
  (define write-shared (with-default-port thcheme:write-shared))
  (define display      (with-default-port thcheme:display))
  ) ; scheme write
-}

writeP, writeSimpleP, writeSharedP, displayP :: Primitive
[writeP, writeSimpleP, writeSharedP, displayP] = map make
  [ ("write", writeSH)
  , ("write-simple", writeSimpleSH)
  , ("write-shared", writeSharedSH)
  , ("display", displaySH)
  ]
  where make (name, writer) = Prim name (Between 1 2) $ mkWriter writer

newlineP :: Primitive
newlineP = Prim "newline" (Between 0 1) newlineB
  where newlineB [] = newlineB [Port pStdout]
        newlineB [v] = do
          p <- unwrapOpenTextualOutputPort v
          liftIO $ pWriteChar '\n' p
          return (Bool True)
        newlineB _ = panic "newline arity"

mkWriter :: (Val -> IO String) -> Builtin
mkWriter writer = builtin
  where
    builtin :: Builtin
    builtin [obj]           = builtin [obj, Port pStdout]
    builtin [obj, v] = unwrapOpenTextualOutputPort v >>= \p -> liftIO $ do
      str <- writer obj
      pWriteString (pack str) p
      return $ MultipleValues []
    builtin _ = panic "write arity"

readContents :: Primitive
readContents = Prim "read-contents" (Exactly 1) $ \case
  [val] -> do
    filename <- unpack <$> unwrapStr val
    contents <- liftIO (readFile filename)
    String <$> newRef (pack contents)
  _ -> panic "readContents arity"

readAll :: Primitive
readAll = Prim "read-all" (Exactly 1) $ \case
  [val] -> do
    filename <- unpack <$> unwrapStr val
    load filename >>= makeMutableList
  _ -> panic "readAll arity"

-------------------------------------------------------------------------------
-- "Unwrappers" that validate ports have certain types
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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM test act = test >>= \b -> unless b act