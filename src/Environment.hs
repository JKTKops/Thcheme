module Environment
    ( Env -- re-exported from LispVal where it does not belong
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVar
    , bindVars
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Maybe (isJust)
import Control.Monad.Except (liftM, liftIO, throwError)

import LispVal (Env, LispErr (..), LispVal (..),
                ThrowsError, IOThrowsError, extractValue, trapError)


isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . Map.lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Get] unbound symbol" var)
          (liftIO . readIORef)
          (Map.lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Set] unbound symbol" var)
          (liftIO . flip writeIORef value)
          (Map.lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (liftIO $ do
              valueRef <- newIORef value
              writeIORef envRef $ Map.insert var valueRef env
              return value)
          (const $ setVar envRef var value >> return value)
          (Map.lookup var env)

bindVar :: Env -> String -> LispVal -> IO Env
bindVar envRef var value = do
    env <- readIORef envRef
    valRef <- newIORef value
    let env' = Map.insert var valRef env
    newIORef env'

bindVars :: Env -> HashMap String LispVal -> IO Env
bindVars envRef bindings = readIORef envRef
                       >>= extendEnv bindings
                       >>= newIORef
  where
    extendEnv :: HashMap String LispVal
              -> HashMap String (IORef LispVal)
              -> IO (HashMap String (IORef LispVal))
    extendEnv bindings env = (`Map.union` env) <$> mapM createRef bindings
    createRef :: LispVal -> IO (IORef LispVal)
    createRef = newIORef
