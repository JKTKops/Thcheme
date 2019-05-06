{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Repl where

import System.IO
import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as Map

import Types
import Parsers
import Evaluation
import Bootstrap

data ReplState = RS { env      :: Env
                    , replOpts :: Opts
                    }

type ReplType = StateT ReplState IO
newtype Repl a = Repl { runReplMonad :: ReplType a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadState ReplState)

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    putStrLn "loaded: stdlib.thm"
    evalStateT (runReplMonad replLoop) $ RS env Map.empty

replLoop :: Repl ()
replLoop = until_
    (isTerminationError . fst)
    (liftIO (prompt ">>> ") >>= replStep)
    (liftIO . putStrLn . showResult)

replStep :: String -> Repl (Either LispErr LispVal, EvalState)
replStep input = do
    replState <- get
    let cEnv = env replState
        cOpts = replOpts replState
    result@(_, evalState) <- liftIO (evaluate cEnv cOpts input)
    put $ replState { replOpts = options evalState }
    return result

until_ :: Monad m
       => (a -> Bool) -- ^ predicate executed on the result of action
       -> m a         -- ^ monadic action which evaluates to the argument for action
       -> (a -> m ()) -- ^ function to call if pred succeeds
       -> m ()        -- ^ Compound action which sequences (prompt >>= action) until pred fails.
until_ pred prompt action = do
    result <- prompt
    if pred result
    then return ()
    else action result >> until_ pred prompt action

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine
