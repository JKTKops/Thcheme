module Environment
    ( Env -- re-exported from Types
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVar
    , bindVars
    , keys
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe (isJust)
import Control.Monad.Except (liftIO, throwError)

-- This module is allowed to import Types because EvaluationMonad
-- imports this module!
import Types

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . Map.lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError Val
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Get] unbound symbol" var)
          (liftIO . readIORef)
          (Map.lookup var env)

setVar :: Env -> String -> Val -> IOThrowsError Val
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Set] unbound symbol" var)
          (liftIO . flip writeIORef value)
          (Map.lookup var env)
    return value

defineVar :: Env -> String -> Val -> IOThrowsError Val
defineVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (liftIO $ do
              valueRef <- newIORef value
              writeIORef envRef $ Map.insert var valueRef env
              return value)
          (const $ setVar envRef var value >> return value)
          (Map.lookup var env)

bindVar :: Env -> String -> Val -> IO Env
bindVar envRef var value = do
    env <- readIORef envRef
    valRef <- newIORef value
    let env' = Map.insert var valRef env
    newIORef env'

bindVars :: Env -> HashMap String Val -> IO Env
bindVars envRef bindings = readIORef envRef
                       >>= extendEnv bindings
                       >>= newIORef
  where
    extendEnv :: HashMap String Val
              -> HashMap String (IORef Val)
              -> IO (HashMap String (IORef Val))
    extendEnv bindings env = (`Map.union` env) <$> mapM createRef bindings
    createRef :: Val -> IO (IORef Val)
    createRef = newIORef

keys :: Env -> IO [String]
keys env = do m <- readIORef env
              return $ Map.keys m