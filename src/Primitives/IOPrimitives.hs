{-# LANGUAGE LambdaCase #-}
module Primitives.IOPrimitives (primitives) where

import System.IO ( IOMode (..)
                 , openFile, hClose, hGetLine, hPutStr, hPutChar
                 , getLine, stdin, stdout)

import Val
import EvaluationMonad
import Parsers (readExpr, load)
import Primitives.Unwrappers (unwrapStr)
import Primitives.WriteLib (writeSH, writeSimpleSH, writeSharedSH, displaySH)

primitives :: [Primitive]
primitives = [ openInputFile
             , openOutputFile
             , closePort
             , readP
             , readLineP
             , writeP, writeSimpleP, writeSharedP, displayP
             , newlineP
             , readContents
             , readAll
             ]

makePort :: Symbol -> IOMode -> Primitive
makePort name mode = Prim name (Exactly 1) $ \case
  [val] -> do
    filename <- unwrapStr val
    Port <$> liftIO (openFile (unpack filename) mode)
  _ -> panic $ "makePort@" ++ symbolAsString name ++ " arity"

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
    [] -> liftIO getLine >>= fmap String . newRef . pack
    _ -> panic "readLine arity"

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
  where newlineB [] = newlineB [Port stdout]
        newlineB [Port hdl] = liftIO $ hPutChar hdl '\n' >> return (Bool True)
        newlineB [badArg] = throwError $ TypeMismatch "port" badArg
        newlineB _ = panic "newline arity"

mkWriter :: (Val -> IO String) -> Builtin
mkWriter writer = builtin
  where
    builtin :: Builtin
    builtin [obj]           = builtin [obj, Port stdout]
    builtin [obj, Port hdl] = liftIO $ do
      str <- writer obj
      hPutStr hdl str
      return $ MultipleValues []
    builtin [_obj, badArg]  = throwError $ TypeMismatch "port" badArg
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
