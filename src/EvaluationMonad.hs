module EvaluationMonad
    (
    -- * re-exported from Types
      EM (..)
    , EvalState (..)
    , StepReason (..)
    , Env
    , Opts

    -- * Re-exported modules
    , module Control.Monad.Cont
    , module Control.Monad.Except
    , module Control.Monad.State.Lazy
    , (Fish.>=>), (Fish.<=<)

      -- * Adjusting the evaluation environment
    , pushEnv, withNewScope, popEnv, envSnapshot

      -- * Adjusting the debug stack
    , pushExpr, popExpr, modifyStackTop, modifyTopReason

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
import Control.Monad (when, void)
import qualified Control.Monad as Fish ((>=>), (<=<))
import Control.Monad.Cont (MonadCont (..))
import Control.Monad.Except (MonadError (..), liftEither, runExceptT)
import Control.Monad.State.Lazy (MonadIO (..), MonadState (..), modify, gets)

import Types
import qualified Environment as Env

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
modifyTopReason f = modifyStackTop (\(r, v) -> (f r, v))

-- | Searches the environment stack top-down for a symbol
getVar :: String -> EM Val
getVar var = do
    stack <- symEnv <$> get
    l <- liftIO $ rights <$> mapM (\env -> runExceptT $ Env.getVar env var) stack
    case l of
        []  -> throwError $ UnboundVar "[Get] unbound symbol" var
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

-- Assumes the first element of args is an Atom; finds it and updates it with given func
updateWith :: ([Val] -> EM Val) -> [Val] -> EM Val
updateWith updater (Atom var : rest) = do
    envRef <- search var
    when (isNothing envRef) $ throwError $ UnboundVar "[Set] unbound symbol" var
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