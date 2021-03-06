{-# LANGUAGE TupleSections #-}
module Evaluation
    ( -- * Initialize an EvalState
      initEvalState
      -- * evaluate a string
    , evaluate
      -- * evaluate a given expression
    , evaluateExpr
      -- * evaluate inside EM monad
    , eval, evalBody, evalTail, evalSeq, evalBodySeq, evalTailSeq
    , runTest
      -- * Convert the evaluation output into a meaningful string
    , showResultIO
      -- * Function application, not sure why this is here rn
    , call, tailCall, rerootDynPoint
    ) where

import Data.Functor (($>))
import Data.List (intercalate)
import Data.IORef (readIORef)
import Control.Monad
import qualified Data.HashMap.Strict as Map

import Parsers
import Val
import EvaluationMonad
import qualified Environment as Env
import Options (ifOpt, Opt(FullStackTrace))
import Primitives.WriteLib (writeSharedSH, showErrIO)

eval :: InTail -- ^ Is this form in tail position of a body?
     -> Val    -- ^ The form to evaluate
     -> EM Val
eval tail expr = do
  fexpr <- freezeList expr
  case fexpr of
    FList (function : args) -> handleApp tail function args
    FList []      -> return Nil -- see Note: [Freezing Nil] in Val.hs
    FNotList obj  -> handleSimpleDatum obj
    FDottedList{} -> throwError $ BadSpecialForm expr

handleSimpleDatum :: Val -> EM Val
handleSimpleDatum obj = case obj of
    (Symbol id)   -> getVar id
    val           -> return val

handleApp :: InTail -- ^ Is this application in tail position?
          -> Val -> [Val] -> EM Val
handleApp tail function args = do
    func <- case function of
        Primitive{}    -> return function
        Continuation{} -> return function
        Closure{}      -> return function
        PrimMacro{}    -> return function
        _              -> evalBody function -- note evalBody, evaluating the
                          -- head of an application can't be a tail eval
    case func of
        Primitive{}    -> evalCall func
        Continuation{} -> evalCall func
        Closure{}      -> evalCall func
        PrimMacro{}    -> evalPMacro func
        MacroTransformer _ transformer -> evalTransformer transformer
        -- TODO: this doesn't play well with the introduction of mutable
        -- lists, so it really is time to make a proper distinction between
        -- functions and low-level macros.
        p@Pair{} -> do
            fp <- freezeList p
            case fp of
                FDottedList [Symbol "macro"] macro@Closure{} -> evalMacro macro
                _ -> evalCall func
        _              -> evalCall func

  where evalCall func = do
            argVals <- mapM evalBody args
                       -- evaluating args also can't be a tail eval
            if tail
            then tailCall func argVals
            else call func argVals

        -- TODO: It's not clear what to do here when in tail position.
        -- My best idea is that PrimMacros need to take an extra arg
        -- that indicates if they are in tail position. This is what's
        -- done here, but it still feels weird.
        -- For user-defined macros, it Just Works (tm) as long as we
        -- correctly eval/tailEval, because the macro will expand into
        -- the current environment and then the expansion should be evaluated
        -- as though the macro was never there.
        evalPMacro pmacro = apply tail pmacro args
        evalTransformer transformer = do
          -- see Note: [keywords in patterns] in Macro.Transformer
          expansion <- transformer (makeImmutableList $ function:args)
          eval tail expansion
        evalMacro macro = do
            expansion <- call macro args
            eval tail expansion

evalBody, evalTail :: Val -> EM Val
evalBody = eval False
evalTail = eval True

evalSeq :: InTail -> [Val] -> EM Val
evalSeq tail = go
  where go [] = error "evalSeq: no forms"
        go [form] = eval tail form
        go (s:ss) = evalBody s >> go ss

-- | Evaluate a sequence in either tail or non-tail position.
-- I'm not sure if 'evalBodySeq' actually has any application,
-- it seems that usually the control of 'evalSeq' is needed.
evalBodySeq, evalTailSeq :: [Val] -> EM Val
evalBodySeq = evalSeq False
evalTailSeq = evalSeq True

call :: Val -> [Val] -> EM Val
call f args = do
  makeStackFrame f args >>= pushFrame
  -- The use of the Applicative <* operator here is absolutely crucial for
  -- call-with-values - if we instead bound the result of apply with '>>=',
  -- f would lose the knowledge that it is allowed to return MultipleValues.
  apply False f args <* popFrame

tailCall :: Val -> [Val] -> EM Val
tailCall f args =
  ifOpt FullStackTrace
    (call f args) -- don't do TCO if FullStackTrace is set
    $ do popFrame
         makeStackFrame f args >>= pushFrame
         apply True f args

apply :: InTail -> Val -> [Val] -> EM Val
-- Applications of primitive functions
apply _ (Primitive arity func _) args = do
  testArity arity args
  func args

apply tail (PrimMacro arity func _) args = do
  testArity arity args
  func tail args

-- Application of continuation
apply _ (Continuation point func) [arg] = do
  rerootDynPoint point
  func arg
apply _ (Continuation point func) args = do
  rerootDynPoint point
  func $ MultipleValues args

-- Applications of user-defined functions
-- We check arity here instead of in 'makeStackFrame' so that this
-- application will be visible on the stack if an error is raised,
-- which makes it look like closures are responsible for checking their
-- own arity.
apply _ (Closure formals mvarg body _cloEnv _name) args = do
                                  -- cloEnv is in the stack frame
  let arity = case mvarg of
        Nothing -> Exactly $ length formals
        Just{}  -> AtLeast $ length formals
  testArity arity args
  evalTailSeq body

apply _ notFunc _ = throwError $ NotFunction "Not a function" notFunc

makeStackFrame :: Val -> [Val] -> EM StackFrame
makeStackFrame (Closure formals mvarg _body cloEnv mname) args = do
  (top, tail) <- case cloEnv of
    [] -> (,[]) <$> liftIO Env.nullEnv
    (top : rest) -> pure (top, rest)

  top'  <- bindFormals top
  top'' <- bindVararg top'
  return $ buildFrame name args $ Just $ top'' : tail
  where
    bindFormals env = liftIO $ Env.bindVars env 
                             $ Map.fromList 
                             $ zip formals args
    bindVararg env = case mvarg of
      Nothing -> return env
      Just name -> do
        varargList <- makeMutableList $ drop (length formals) args
        liftIO $ Env.bindVar env name varargList
    name = maybe "#<closure>" symbolName mname
makeStackFrame (Primitive _ _ name) args = pure $ buildFrame name args Nothing
makeStackFrame (PrimMacro _ _ name) args = pure $ buildFrame name args Nothing
makeStackFrame Continuation{} args = pure $ buildFrame "#<cont>" args Nothing
makeStackFrame head args = 
  pure $ StackFrame (makeImmutableList (head:args)) Nothing

buildFrame :: Symbol -> [Val] -> Maybe LocalEnv -> StackFrame
buildFrame name args = StackFrame (makeImmutableList (Symbol name : args))

-- This function (awkwardly) needs to be here because it needs 'call'.
rerootDynPoint :: DynamicPoint -> EM ()
rerootDynPoint Sentinel = panic "rerootDynPoint: sentinel!"
rerootDynPoint there@(Point dataRef parentRef) = do
  hereRef <- gets dynPoint
  here <- readRef hereRef
  unless (here == there) $ do
    readRef parentRef >>= rerootDynPoint
    here <- readRef hereRef
    let Point hereDataRef hereParentRef = here
    (before, after) <- readRef dataRef
    writeRef hereDataRef (after, before)
    writeRef hereParentRef there
    writeRef dataRef (Nil, Nil)
    writeRef parentRef Sentinel
    writeRef hereRef there
    _ <- call before []
    return ()

-------------------------------------------------------------------------------
-- Primary entrypoints
-------------------------------------------------------------------------------

-- TODO: Currently these just open an InputTextPort via 'labeledReadExpr'
-- but it would be better to make the input be a port and then track these
-- back to the use sites and give a port instead of whatever String IO is
-- being used currently.
-- Actually, there's a problem with that: the REPL needs to output its
-- prompt and keep the input buffer clear between invocations. However,
-- The REPL can simply produce an InputTextPort to hand over.

evaluate :: String -> EvalState -> String -> IO (Either LispErr Val, EvalState)
evaluate label state input = labeledReadExpr label input >>= \case
  Right expr -> evaluateExpr state expr
  Left e     -> pure (Left e, state)

-- | Evaluate an expression at the top level using the given initial EvalState.
-- At the end of evaluation, the dynamic environment is guaranteed to be the
-- same as the dynamic environment at the start, even if evaluating the body
-- causes early termination via exceptions.
--
-- Does some minor linting of the 'EvalState' after evaluation which will fail
-- if the 'EvalState' does not represent a top-level dynamic environment.
evaluateExpr :: EvalState -> Val -> IO (Either LispErr Val, EvalState)
evaluateExpr state v = do
  initialDynPoint <- readIORef $ dynPoint state
  r <- execEM state $ evalBody v
  -- note that we take special care to reroot outside of the same execEM
  -- call that evaluates v. If 'evalBody v' throws an error, we want to
  -- be sure that this reroot still happens. After all, if it doesn't
  -- throw an error, this reroot won't do anything!
  -- Returning 'Undefined' is just an arbitrary choice of 'Val' to
  -- satisfy the type of execEM.
  
  -- another note: any calls to initEvalState during the execution of
  -- evalBody v would make a new dynamic root, and reroots from there
  -- would not be reflected here. Thus it's critical that
  -- 'inInteractionEnv' in Primitives.Misc does not create a new root.
  -- It can use the current dynamic point or reroot to another point
  -- that belongs to the same tree, but it /cannot/ create a /new/ root.
  _ <- execEM state $ rerootDynPoint initialDynPoint $> Undefined
  _ <- execEM state $ lintTopDynEnv initialDynPoint
  return r

-- takes the initial dynamic point so that it can check that the current
-- point is both the same and the root.
lintTopDynEnv :: DynamicPoint -> EM Val
lintTopDynEnv initialDynPoint
  = do lintAssert "dynamic environment is not initial after evaluation" check
       return Undefined
  where
    check = callCC $ \exit -> do
      currentDynPoint <- gets dynPoint >>= readRef
      unless (currentDynPoint == initialDynPoint) $ exit False
      let Point _ parentRef = initialDynPoint
      parent <- readRef parentRef
      case parent of
        Sentinel -> pure ()
        _        -> exit False

      handlersRef <- gets currentHandlers
      handlers <- readRef handlersRef
      case handlers of
        [_] -> pure ()
        _   -> exit False
      return True -- end lintAssert


-- | Provided for backwards compatibility.
runTest :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
runTest env opts m = do
  s <- initEvalState env opts
  execEM s m

showResultIO :: (Either LispErr Val, EvalState) -> IO String
showResultIO res = case res of
    (Left e@(Parser _), _) -> pure $ show e ++ "\n"
    (Left err, s) -> diffLines <$> showErrIO err <*> showEvalState s
    (Right (MultipleValues vs), _) -> do
      strs <- mapM writeSharedSH vs
      return $ intercalate "\n" strs
    (Right v, _) -> writeSharedSH v
  where diffLines s r = s ++ "\n" ++ r

-- I'd rather this was in EvaluationMonad, but this is almost as good.
-- In all honestly, there's a real argument that it belongs in Repl.
-- If Thcheme ever becomes a compiler, this does need to be bundled with
-- main to display any errors that the program raises and doesn't catch.
-- If that ever happens, then it stops really mattering, because all of
-- Thcheme would need to be in the Thcheme library, right? Maybe that's not
-- right. Hmm.
showEvalState :: EvalState -> IO String
showEvalState es = ("Stack trace:\n" ++) <$> numberedLines
  where numberedLines :: IO String
        -- unlines puts an extra newline at the end, which we
        -- actually want because it looks better.
        numberedLines = unlines . map (limit 100) . zipWith (<+>) numbers <$> exprs
        numbers = map (\i -> show i ++ ";") [1 :: Int ..]

        -- Note that we use write-shared here. It's possible that the error
        -- was raised because we tried to 'eval' a cyclic list. If we don't
        -- use write-shared, that will make the program hang.
        --
        -- Optimally, we would probably get information from showResultIO
        -- about whether or not we need to worry about that; the error itself
        -- is always displayed using write-shared for values it contains.
        -- However, we still need to be careful! Trying to evaluate cyclic
        -- _data_ won't crash (in fact, the program it represents could even
        -- be a correct, terminating program). Attempting to write-simple
        -- cyclic data will still hang, and write-shared is actually more
        -- efficient than write!
        exprs = forM (stack es) $ \(StackFrame form _) -> writeSharedSH form

        limit :: Int -> String -> String
        limit _ "" = ""
        limit n s
          | n < 3 = s
          | otherwise = loop n s
          where
            loop 3 s = case s of
              -- len s >= 4
              (_:_:_:_:_) -> "..."
              _ -> s
            loop _ "" = ""
            loop n (c:cs) = c : loop (n-1) cs

(<+>) :: String -> String -> String
"" <+> s  = s
s  <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2
