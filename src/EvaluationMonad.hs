module EvaluationMonad
    (
    -- * re-exported from Types
      EM (..)
    , EvalState (..)
    , StepReason (..)
    , Env
    , Opts

    -- * Re-exported modules
    , callCC -- only useful export of Control.Monad.Cont
    , module Control.Monad.Except
    , module Control.Monad.State.Lazy
    , (Fish.>=>), (Fish.<=<)

      -- * Execute EM actions
    , execEM, execAnyEM, unsafeEMtoIO

      -- * Adjusting the evaluation environment
    , pushEnv, withNewScope, popEnv, envSnapshot

      -- * Adjusting the debug stack
    , pushExpr, popExpr, modifyStackTop, modifyTopReason

      -- * using options
    , enableOpt, disableOpt, lintAssert, noOpts

      -- * explode
    , panic

      -- * Use the evaluation environment
    , getVar, setVar, updateWith, search
    , setVarForCapture, defineVar

      -- * Manipulating IORefs
    , newRef, readRef, writeRef, modifyRef
    ) where

import Data.IORef
import Data.Maybe
import Data.Either
import qualified Data.HashMap.Strict as Map

import Control.Arrow (first)
import Control.Monad (when, unless, void)
import qualified Control.Monad as Fish ((>=>), (<=<))
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.State.Lazy (MonadIO (..), MonadState (..), modify, gets)

import Options
import Types
import qualified Environment as Env


execEM :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
execEM initEnv opts (EM m) = runCont m (\v s -> pure (Right v, s)) $
                                ES [] [initEnv] opts

-- | Useful for testing etc.
execAnyEM :: Env -> Opts -> EM a -> IO (Either LispErr a)
execAnyEM env opts m = do
  -- This hack looks very weird if you don't know what's going on here.
  -- See Note: [EM return types] in Types.hs.
  store <- newIORef (error "execAnyEM: forced store")
  (either, _) <- execEM env opts $ do
    a <- m
    writeRef store a
    return Undefined
  case either of
    Right _ -> Right <$> readIORef store
    Left e  -> return $ Left e

-- | Convert an EM action to an IO action.
--
-- This function is unsafe in the sense that it doesn't handle errors
-- raised in the EM action, so the action should be guaranteed not to
-- raise errors. This function is exposed mainly for the test suite,
-- which uses it to access 'equalSSH' as an IO action.
--
-- The EM action is run with linting enabled.
unsafeEMtoIO :: EM a -> IO a
unsafeEMtoIO em = do
  ne <- Env.nullEnv
  Right a <- execAnyEM ne (setOpt Lint noOpts) em
  return a

-- | Push an expr to the call stack
pushExpr :: StepReason -> Val -> EM ()
pushExpr r val = do
    s <- get
    put $ s { stack = (r, val) : stack s }

-- | Remove the top item from the call stack
popExpr :: EM ()
popExpr = modify $ \s -> s { stack = case stack s of
    []     -> error "popExpr: empty stack"
    (_:tl) -> tl
  }

-- | Push an environment to the top of the sEnv stack
--   The environment becomes the topmost scope of evaluation.
pushEnv :: Env -> EM ()
pushEnv e = modify $ \s -> s { symEnv = e : symEnv s }

-- | Push a new empty scope to the sEnv stack.
pushEmptyEnv :: EM ()
pushEmptyEnv = do
    e <- liftIO $ newIORef Map.empty
    pushEnv e

-- | Evaluate an EM action in an empty local scope.
withNewScope :: EM a -> EM a
withNewScope m = pushEmptyEnv *> m <* popEnv

-- | Remove the topmost scope from the sEnv stack.
popEnv :: EM ()
popEnv = do
    s <- get
    let stack = symEnv s
    put $ s { symEnv = case stack of
                [] -> error "popEnv: empty env stack"
                (_:tl) -> tl
            }

envSnapshot :: EM Env
envSnapshot = do
    symEnvStack <- gets symEnv
    liftIO $ flatten symEnvStack >>= newIORef
  where
    flatten = fmap mconcat . mapM readIORef

modifyStackTop :: ((StepReason, Val) -> (StepReason, Val)) -> EM ()
modifyStackTop f = modify $ \s ->
    let top : tail = stack s in
    s { stack = f top : tail }

modifyTopReason :: (StepReason -> StepReason) -> EM ()
modifyTopReason f = modifyStackTop $ first f

-- | If linting is enabled, assert that a predicate is true.
-- If the assertion fails, the runtime will panic with the given message.
lintAssert :: (Monad m, HasOpts m) => String -> m Bool -> m ()
lintAssert msg test = whenOpt Lint $ test >>= \b ->
  unless b $ panic $ panicmsg ++ msg
  where panicmsg = "\nLinter detected an invariant violation:\n"

-- | Panic.
--
-- Calling this function throws a synchronous Haskell exception, printing
-- the given message along with a note about "the impossible" happening.
panic :: String -> a
panic msg = error $ "Panic! The \"impossible\" happened.\n" ++ msg

-- | Searches the environment stack top-down for a symbol
getVar :: String -> EM Val
getVar var = do
    stack <- symEnv <$> get
    l <- liftIO $ rights <$> mapM (\env -> runExceptT $ Env.getVar env var) stack
    case l of
        []  -> throwError $ UnboundVar "[Get]" var
        Undefined : _ -> throwError $ EvaluateDuringInit var
        v:_ -> return v

-- | Search the environment top-down for a symbol
--   If it's found, bind it to the given Val
--   Otherwise, create a new binding in the top-level
setVar :: String -> Val -> EM Val
setVar var val = do
    mEnv <- search var
    case mEnv of
        Nothing -> defineVar var val
        Just e  -> do
            Right v <- liftIO . runExceptT $ Env.defineVar e var val
            return v

-- Assumes the first element of args is an Atom;
-- finds it and updates it with given func
updateWith :: ([Val] -> EM Val) -> [Val] -> EM Val
updateWith updater (Atom var : rest) = do
    envRef <- search var
    when (isNothing envRef) $ throwError $ UnboundVar "[Set]" var
    env <- liftIO . readIORef $ fromJust envRef
    let ref = fromJust $ Map.lookup var env
    val <- liftIO $ readIORef ref
    updated <- updater (val : rest)
    liftIO $ writeIORef ref updated
    return updated

search :: String -> EM (Maybe Env)
search var = do
    stack <- symEnv <$> get
    l <- liftIO $ catMaybes <$> mapM
         (\env -> do
                 e <- runExceptT $ Env.getVar env var
                 return $ if isRight e then Just env else Nothing
         ) stack
    return $ case l of
        []  -> Nothing
        e:_ -> Just e

-- | Define a var in the top-level environment in preparation for
-- a function being 'define'd to capture itself.
setVarForCapture :: String -> EM ()
setVarForCapture name = void $
  defineVar name Undefined

defineVar :: String -> Val -> EM Val
defineVar var val = do
    env <- head . symEnv <$> get
    liftIOThrows $ Env.defineVar env var val

newRef :: a -> EM (IORef a)
newRef = liftIO . newIORef
{-# INLINE newRef #-}

readRef :: IORef a -> EM a
readRef = liftIO . readIORef
{-# INLINE readRef #-}

writeRef :: IORef a -> a -> EM ()
writeRef ref x = liftIO $ writeIORef ref x
{-# INLINE writeRef #-}

modifyRef :: IORef a -> (a -> a) -> EM ()
modifyRef ref f = liftIO $ modifyIORef ref f
{-# INLINE modifyRef #-}
