{-# LANGUAGE LambdaCase #-}
module Primitives.Misc (primitives, macros, valuesB) where

-- TODO split into Control and Syntax
import Text.Read (readMaybe)

import Parsers (load)
import Val
import Evaluation
import EvaluationMonad
import Primitives.String (stringSH, unwrapStringPH)
import Control.Monad.Reader -- for qq

-- TODO NEXT: this module is broken right now because of Pairs

primitives :: [Primitive]
primitives = [ identityFunction
             , evalP
             , applyP
             , quit
             , loadP
             , callWithCurrentContinuation
             , dynamicWind
             , valuesP
             , callWithValues
             ]

macros :: [(String, Macro)]
macros = [ ("quote", quote)
         , ("quasiquote", quasiquote)
         , ("if", ifMacro)
         , ("set!", set)
         , ("set-option!", setOpt)
         , ("define", define)
         , ("lambda", lambda)
         , ("defmacro", defmacro)
         , ("macroexpand", macroexpand)
         , ("begin", begin)
         ]

identityFunction :: Primitive
identityFunction = Prim "id" (Exactly 1) $ \[arg] -> return arg

callWithCurrentContinuation :: Primitive
callWithCurrentContinuation = 
  Prim "call-with-current-continuation" (Exactly 1) callcc
  where
    callcc :: [Val] -> EM Val
    callcc [func] = do
      here <- gets dynPoint >>= readRef
      callCC $ \k ->
        tailCall func [Continuation here k]
    callcc _ = panic "callcc arity"

dynamicWind :: Primitive
dynamicWind = Prim "dynamic-wind" (Exactly 3) $ 
  \[before, during, after] -> do
    here <- gets dynPoint >>= readRef
    let procs = (before, after)
    procRef   <- newRef procs
    parentRef <- newRef here
    let point = Point procRef parentRef
    rerootDynPoint point
    dv <- call during []
    rerootDynPoint here
    return dv

valuesP :: Primitive
valuesP = Prim "values" (AtLeast 0) valuesB

valuesB :: Builtin
valuesB [arg] = return arg
valuesB args  = return $ MultipleValues args
{-# INLINE valuesB #-}

callWithValues :: Primitive
callWithValues = Prim "call-with-values" (Exactly 2) $
  \[producer, consumer] -> do
    result <- call producer []
    case result of
      MultipleValues vs -> tailCall consumer vs
      singleVal -> tailCall consumer [singleVal]

-- compose?

quote :: Macro
quote = Macro (Exactly 1) $ \_ [form]  -> return form

ifMacro :: Macro
ifMacro = Macro (Between 2 3) $ 
  \tail (pred : conseq : alts) -> do
    p <- evalBody pred
    if truthy p
    then eval tail conseq
    else case alts of
      [] -> return Nil-- r7rs says unspecified
      [x] -> eval tail x
      _ -> panic "if arity"

set :: Macro
set = Macro (Exactly 2) $ \_ -> \case
  [Symbol var, form] -> evalBody form >>= setVar var
  [notAtom, _]     -> throwError $ TypeMismatch "symbol" notAtom
  _ -> panic "set arity"

setOpt :: Macro
setOpt = Macro (Exactly 2) $ \_ -> \case
  [Symbol optName, form] -> do
    val <- evalBody form
    let mopt = readMaybe optName
    case mopt of
      Nothing -> pure ()
      Just opt -> if truthy val then enableOpt opt else disableOpt opt
    return val
  [notAtom, _] -> throwError $ TypeMismatch "symbol" notAtom
  _ -> panic "setOpt arity"

defineB :: Builtin
-- arity is AtLeast 1 so can't be called with no args
defineB [] = panic "define arity"
defineB (x:xs) = do
  defnHead <- freezeList x
  defineBuiltinFrozen defnHead xs

  where
    defineBuiltinFrozen :: FrozenList -> [Val] -> EM Val
    defineBuiltinFrozen (FNotList (Symbol var)) [form] = do
      setVarForCapture var
      val <- evalBody form
      let renamed = case val of
            Closure{} -> val { name = Just var }
            _ -> val
      defineVar var renamed
    defineBuiltinFrozen (FNotList (Symbol var)) badForms =
      throwError $ NumArgs (Exactly 2) (Symbol var : badForms)
    
    defineBuiltinFrozen (FList (Symbol name : params)) body = case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body (Just name) >>= defineVar name
    
    defineBuiltinFrozen (FDottedList (Symbol name : params) varargs) body = 
      case body of
        [] -> throwError emptyBodyError
        _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name
    
    defineBuiltinFrozen (FList (notAtom : _)) _ = 
      throwError $ TypeMismatch "symbol" notAtom
    
    defineBuiltinFrozen (FDottedList (notAtom : _) _) _ =
      throwError $ TypeMismatch "symbol" notAtom
    
    defineBuiltinFrozen _notAtomOrList _ = 
      throwError $ TypeMismatch "symbol or list" x

-- arities are AtLeast 1 so that we can throw a nicer error than just
-- "expects at least 2 arguments" in the case of defining a function.
-- 'define's builtin throws the regular Exactly 2 error if it isn't defining
-- a function.
define, lambda :: Macro
define = Macro (AtLeast 1) $ const defineB
lambda = Macro (AtLeast 1) $ const lambdaB

lambdaB :: Builtin
-- arity is AtLeast 1 so can't be called with no args
lambdaB [] = panic "lambda arity"
lambdaB (formals : body) = do 
  frozenFormals <- freezeList formals
  case frozenFormals of
    FList params -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncNormal params body Nothing
    FDottedList params varargs -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs params body Nothing
    FNotList varargs@(Symbol _) -> case body of
      [] -> throwError emptyBodyError
      _  -> makeFuncVarargs varargs [] body Nothing
    _notAtomOrList -> throwError $ TypeMismatch "symbol or list" formals

emptyBodyError :: LispErr
emptyBodyError = EmptyBody -- historical artifact, feel free to inline

makeFunc :: Maybe String
         -> [Val]
         -> [Val]
         -> Maybe String
         -> EM Val
makeFunc varargs params body name = do
    maybe (pure ()) setVarForCapture name
    lcl <- gets localEnv
    return $ Closure (map show params) varargs body lcl name

makeFuncNormal :: [Val] -> [Val] -> Maybe String -> EM Val
makeFuncNormal = makeFunc Nothing
makeFuncVarargs :: Val -> [Val] -> [Val] -> Maybe String -> EM Val
makeFuncVarargs = makeFunc . Just . show

defmacro :: Macro
defmacro = Macro (AtLeast 2) $ const $
  \(Symbol name : ps : body) -> do
    lam <- lambdaB (ps : body)
    macro <- makeImproperMutableList [Symbol "macro"] lam
    defineVar name macro

macroexpand :: Macro
macroexpand = Macro (Exactly 1) $ const macroExpandB

macroExpandB :: [Val] -> EM Val
macroExpandB [syntaxForm] = do
  (Symbol keyword : args) <- getListOrError syntaxForm
  macro <- evalBody (Symbol keyword)
  case macro of
    MacroTransformer _ transformer -> 
      expandTransformer transformer (Symbol keyword:args)
    p@Pair{} -> do
      fp <- freezeList p
      case fp of
        FDottedList [Symbol "macro"] macro@Closure{} -> expandMacro macro args
        _ -> notMacro
    PrimMacro{} -> throwError $
      Default $ "can't expand primitive macro " ++ keyword
    _ -> notMacro
  where
    notMacro = throwError $ TypeMismatch "macro" syntaxForm
    -- see Note: [keywords in patterns] in Macro.Transformer
    expandTransformer transformer forms = transformer (makeImmutableList forms)
    expandMacro macro args = call macro args
macroExpandB _ = panic "macroexpand arity"


begin :: Macro
begin = Macro (AtLeast 1) $ \tail stmts -> evalSeq tail stmts

-- | In @inInteractionEnv m@, the continuation of m is the
-- continuation of inInteractionEnv. However, 'm' is executed
-- in the EvalState that represents the top level.
inInteractionEnv :: EM a -> EM a
inInteractionEnv m = do
  state <- get
  -- put (interaction-environment)
  -- note that this will execute m outside
  -- the dynamic extent of the caller but without moving
  -- dynamic points; merely using a new root.
  -- I'm pretty sure this is wrong. We probably
  -- really want to reroot the dynamic points to the
  -- original root before evaluating and then reroot back
  -- after, or use the current dynamic point.
  -- edit: actually, I think this is unspecified, and this
  -- behavior is probably what I would predict in the wild.
  init <- liftIO $ initEvalState (globalEnv state) (options state)
  put init
  m <* put state

evalP :: Primitive
evalP = Prim "eval" (Exactly 1) $ \[form] -> inInteractionEnv $ evalBody form

-- TODO: [r7rs]
applyP :: Primitive
applyP = Prim "apply" (AtLeast 2) $
  \(func : rest@(_:_)) -> do
    args <- processApplyArgs rest
    tailCall func args
  where
    processApplyArgs [form] = do
      frozenForm <- freezeList form
      case frozenForm of
        FList args -> return args
        _bad -> throwError $ TypeMismatch "list" form
    processApplyArgs (arg : args) = do
      tl <- processApplyArgs args
      return $ arg : tl
    processApplyArgs [] = panic "apply arity"

loadP :: Primitive
loadP = Prim "load" (Exactly 1) $ \case
  [val] | stringSH val -> do
          filename <- unwrapStringPH val
          file <- liftIO . runExceptT $ load filename
          case file of
            Left e   -> throwError e
            Right ls -> inInteractionEnv $ evalBodySeq ls
        | otherwise -> throwError $ TypeMismatch "string" val
  _ -> panic "load arity"

quasiquote :: Macro
quasiquote = Macro (Exactly 1) $ \_ [form]-> runReaderT (qq form) 0
  where 
    qq :: Val -> ReaderT Int EM Val
    qq term = lift (freezeList term) >>= \case
      FList [Symbol "quasiquote", form] -> local (+ 1) $ do
        inner <- qq form
        lift $ makeMutableList [Symbol "quasiquote", inner]
      FList [Symbol "unquote", form] -> do
        depth <- ask
        if depth == 0
        then lift $ evalBody form
        else local (subtract 1) $ do
          inner <- qq form
          lift $ makeMutableList [Symbol "unquote", inner]
      FList forms
          -- this can happen because the parser is smart enough to rewrite
          -- `(0 . ,lst) as IList [Number 0, Symbol unquote, Symbol lst]
          -- however whenever this happens, ',lst' will definitely be of size 2
        | Symbol "unquote" `elem` forms
        , (list, dot) <- break (== Symbol "unquote") forms
        , [_,_] <- dot
        -> do lift (makeMutableList dot) >>= qqImproper list
        | otherwise
        -> qqTerms forms >>= lift . makeMutableList
      FDottedList forms form -> qqImproper forms form
      _other -> return term

    qqTerms :: [Val] -> ReaderT Int EM [Val]
    qqTerms [] = return []
    qqTerms (t:ts) = do
      frozenT <- lift $ freezeList t
      terms <- case frozenT of
        FList [Symbol "unquote-splicing", form] -> do
          depth <- ask
          val <- if depth == 0
                 then lift $ evalBody form
                 else local (subtract 1) $ qq form
          lift $ getListOrError val
        _ -> (:[]) <$> qq t
      (terms ++) <$> qqTerms ts
    
    qqImproper :: [Val] -> Val -> ReaderT Int EM Val
    qqImproper prop improp = do
      qqprop <- qqTerms prop
      qqimprop <- qq improp
      lift $ makeImproperMutableList qqprop qqimprop

quit :: Primitive
quit = Prim "quit" (Exactly 0) $ \_ -> throwError Quit
