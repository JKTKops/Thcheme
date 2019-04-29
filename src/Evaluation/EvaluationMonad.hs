{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Evaluation.EvaluationMonad where

import Data.Maybe
import Data.Either
import Control.Monad.Except
import Control.Monad.State.Lazy

import LispVal
import Environment as Env

-- | The Evaluation Monad
type EMt = StateT EvalState IOThrowsError
newtype EM a = EM { runEM :: EMt a }
  deriving (Monad, Functor, Applicative, MonadIO
           , MonadError LispErr, MonadState EvalState)

-- | The current state of evaluation
data EvalState = ES { expr       :: LispVal
                    , sEnv       :: [Env]
                    , quoteLevel :: Int
                    }

initState :: LispVal -> EvalState
initState expr = ES expr [] 0

evaluate :: EM a -> LispVal -> IO (Either LispErr a)
evaluate em = runExceptT . evalStateT (runEM em) . initState

-- | Set the expr being evaluated
putExpr :: LispVal -> EM ()
putExpr val = do
    s <- get
    put $ s { expr = val }

-- | Push an environment to the top of the sEnv stack
--   The environment becomes the topmost scope of evaluation.
pushEnv :: Env -> EM ()
pushEnv e = do
    s <- get
    let stack = sEnv s
    put $ s {sEnv = e : stack }

-- | Remove the topmost scope from the sEnv stack.
popEnv :: EM ()
popEnv = do
    s <- get
    let stack = sEnv s
    put $ s { sEnv = tail stack }

-- | Searches the environment stack top-down for a symbol
getVar :: String -> EM LispVal
getVar var = do
    stack <- sEnv <$> get
    EM . lift $ do
        l <- liftIO $ rights <$> mapM (\env -> runExceptT $ Env.getVar env var) stack
        case l of
            []  -> throwError $ UnboundVar "[Get] unbound symbol" var
            v:_ -> return v

-- | Search the environment top-down for a symbol
--   If it's found, bind it to the given LispVal
--   Otherwise, create a new binding in the top-level
setVar :: String -> LispVal -> EM LispVal
setVar var val = do
    mEnv <- search var
    case mEnv of
        Nothing -> throwError $ UnboundVar "[Set] unbound symbold" var
        Just e  -> EM . lift $ Env.defineVar e var val

search :: String -> EM (Maybe Env)
search var = do
    stack <- sEnv <$> get
    EM . lift $ do
        l <- liftIO $ catMaybes <$> mapM
             (\env -> do
                   e <- runExceptT $ Env.getVar env var
                   return $ if isRight e then Just env else Nothing
             ) stack
        return $ case l of
            []  -> Nothing
            e:_ -> Just e

defineVar :: String -> LispVal -> EM LispVal
defineVar var val = do
    env <- head . sEnv <$> get
    EM . lift $ Env.defineVar env var val
