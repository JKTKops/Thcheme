{-# LANGUAGE TupleSections #-}
module Evaluation
    ( -- | Initialize an EvalState
      initEvalState
      -- | evaluate a string
    , evaluate
      -- | evaluate a given expression
    , evaluateExpr
      -- | evaluate inside EM monad
    , eval, evalBody, evalTail, evalSeq, evalBodySeq, evalTailSeq
    , runTest
      -- | Convert the evaluation output into a meaningful string
    , showResultIO
      -- | Function application, not sure why this is here rn
    , call, tailCall, rerootDynPoint
    ) where

import Data.List (intercalate)
import Data.Maybe
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
apply _ (Continuation point One func) [arg] = do
  rerootDynPoint point
  func arg
apply _ (Continuation point Any func) args = do
  rerootDynPoint point
  func $ MultipleValues args
apply _ Continuation{} badArgs = throwError $ NumArgs (Exactly 1) badArgs

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
    name = fromMaybe "#<closure>" mname
makeStackFrame (Primitive _ _ name) args = pure $ buildFrame name args Nothing
makeStackFrame (PrimMacro _ _ name) args = pure $ buildFrame name args Nothing
makeStackFrame Continuation{} args = pure $ buildFrame "#<cont>" args Nothing
makeStackFrame head args = 
  pure $ StackFrame (makeImmutableList (head:args)) Nothing

buildFrame :: String -> [Val] -> Maybe LocalEnv -> StackFrame
buildFrame name args = StackFrame (makeImmutableList (Symbol name : args))

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
evaluate :: String -> EvalState -> String -> IO (Either LispErr Val, EvalState)
evaluate label state input = execEM state $
  liftEither (labeledReadExpr label input) >>= allowMultipleValues . evalBody

evaluateExpr :: EvalState -> Val -> IO (Either LispErr Val, EvalState)
evaluateExpr state v = execEM state $ evalBody v

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
        numberedLines = unlines . zipWith (<+>) numbers <$> exprs
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

(<+>) :: String -> String -> String
"" <+> s  = s
s  <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2
