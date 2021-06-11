{-# LANGUAGE LambdaCase #-}
-- TODO: rename to just IO
module Primitives.IOPrimitives (primitives) where

import Val
import EvaluationMonad
import Parsers (load)
import Parser.Parsec (readDatum)
import Types.Port
import Types.Unwrappers
    ( unwrapStr,
      unwrapOpenTextualInputPort,
      unwrapOpenTextualOutputPort )
import Primitives.WriteLib (writeSH, writeSimpleSH, writeSharedSH, displaySH)

primitives :: [Primitive]
primitives = [ readP
             , readLineP
             , writeP, writeSimpleP, writeSharedP, displayP
             , newlineP
             , readContents
             , readAll
             ]

readP :: Primitive
readP = Prim "read" (Between 0 1) read
  where
    read :: Builtin
    read []  = read [Port pStdin]
    read [v] = do
      p <- unwrapOpenTextualInputPort v
      let ev = case readDatum "read" p of
                 Left str -> Left $ Parser str
                 Right v  -> Right v
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
~~We don't define or use 'current-input-port' or any of the
port operations like 'with-output-string' or whatever it's called.
We can/should split these operations into Port.hs and Display.hs or something
to that effect.~~
^^ still have to define current-input-port but splitting and port operations
happened already.

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
