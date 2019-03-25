module Environment 
    ( Env -- re-exported from LispVal where it does not belong
    , IOThrowsError
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    , nullEnv
    , primitiveBindings
    , liftThrows
    , runIOThrows
    ) where

import Data.IORef
import Control.Monad.Except (liftM, liftIO, throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

import Primitives (primitives)
import LispVal (Env, LispErr (..), LispVal (..), ThrowsError, extractValue, trapError)

type IOThrowsError = ExceptT LispErr IO

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Get] unbound symbol" var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "[Set] unbound symbol" var)
          (liftIO . (flip writeIORef) value)
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (liftIO $ do
              valueRef <- newIORef value
              writeIORef envRef $ (var, valueRef) : env
              return value)
          (const $ setVar envRef var value >> return value)
          (lookup var env)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef
                       >>= extendEnv bindings
                       >>= newIORef where
    extendEnv :: [(String, LispVal)] 
              -> [(String, IORef LispVal)] 
              -> IO [(String, IORef LispVal)]
    extendEnv bindings env = liftM (++ env) $ mapM createRef bindings
    createRef :: (String, LispVal) -> IO (String, IORef LispVal)
    createRef (var, value) = do ref <- newIORef value
                                return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitive primitives)
    where makePrimitive (name, func) = (name, Primitive func name)
    
liftThrows :: ThrowsError a -> IOThrowsError a
           -- (MonadError m a) => Either e a -> m a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT action >>= return . extractValue . trapError
