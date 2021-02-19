module EvaluationMonad
    (
    -- * re-exported from Types
      EM (..)
    , EvalState (..)
    , StackFrame (..)
    , Env, GlobalEnv, LocalEnv
    , Opts

    -- * Re-exported modules
    , callCC -- only useful export of Control.Monad.Cont
    , module Control.Monad.Except
    , module Control.Monad.State.Lazy
    , (Fish.>=>), (Fish.<=<)

      -- * Execute EM actions
    , execEM, execAnyEM, unsafeEMtoIO

      -- * Adjusting the evaluation environment
    , pushFrame, popFrame

      -- * using options
    , enableOpt, disableOpt, lintAssert, noOpts

      -- * explode
    , panic

      -- * Use the evaluation environment
    , getVar, setVar, search
    , setVarForCapture, defineVar

      -- * Manipulating IORefs
    , newRef, readRef, writeRef, modifyRef
    ) where

import Data.IORef
import Data.Either
import Data.Functor ((<&>))

import Control.Monad (unless, void)
import qualified Control.Monad as Fish ((>=>), (<=<))
import Control.Monad.Cont (callCC, runCont)
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.State.Lazy (MonadIO (..), MonadState (..), modify, gets)

import Options
import Types
import qualified Environment as Env

import GHC.Stack (HasCallStack) -- for panic


execEM :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
execEM initEnv opts (EM m) = runCont m (\v s -> pure (Right v, s)) $
                                ES initEnv [] [] opts

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

pushFrame :: StackFrame -> EM ()
pushFrame frame@(StackFrame _ mlcl) = modify $ \s -> case mlcl of
  Just lcl -> s { localEnv = lcl, stack = frame : stack s }
  Nothing  -> s { localEnv = [],  stack = frame : stack s }

popFrame :: EM ()
popFrame = do
  frameStack <- gets stack
  case frameStack of
    [] -> panic "popFrame: no frame"
    [_] -> modify $ \s -> s { localEnv = [], stack = [] }
    (_ : tail@(StackFrame _ mOldLcl : _)) ->
      modify $ \s -> case mOldLcl of
        Just lcl -> s { localEnv = lcl, stack = tail }
        Nothing  -> s { localEnv = [],  stack = tail }

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
panic :: HasCallStack => String -> a
panic msg = error $ "Panic! The \"impossible\" happened.\n" ++ msg

-- | Searches the environment stack top-down for a symbol
getVar :: String -> EM Val
getVar var = do
    gbl <- gets globalEnv
    lcl <- gets localEnv
    v   <- loop gbl lcl
    crashUndefined v
  where
    loop gbl [] = liftIOThrows $ Env.getVar gbl var
    loop gbl (env:envs) = do
      ev <- liftIO $ runExceptT $ Env.getVar env var
      case ev of
        Left _  -> loop gbl envs
        Right v -> return v
    
    crashUndefined Undefined = throwError $ EvaluateDuringInit var
    crashUndefined v = return v

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

search :: String -> EM (Maybe Env)
search var = do
    lcl <- gets localEnv
    me <- loop lcl
    case me of
      Nothing -> tryGbl
      Just _e -> return me
  where
    loop [] = return Nothing
    loop (env:envs) = do
      e <- liftIO $ runExceptT $ Env.getVar env var
      if isRight e then return $ Just env else loop envs
    
    tryGbl = do
      gbl <- gets globalEnv
      liftIO $ runExceptT (Env.getVar gbl var) <&> \case
        Right _ -> Just gbl
        Left _  -> Nothing

-- | Define a var in the innermost environment in preparation for
-- a function being 'define'd to capture itself.
setVarForCapture :: String -> EM ()
setVarForCapture name = void $
  defineVar name Undefined

-- | Define a var in the innermost environment.
--
-- Use 'setVar' to set a local var that might not be from the innermost
-- environment.
defineVar :: String -> Val -> EM Val
defineVar var val = do
    lclStack <- gets localEnv
    env <- case lclStack of
      (e:_) -> pure e
      [] -> gets globalEnv
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
