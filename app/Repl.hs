{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Repl where

import System.IO
import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as Map

import qualified System.Console.Haskeline as CLI

import Types
import Parsers
import Evaluation
import Bootstrap

data ReplState = RS { env      :: Env
                    , replOpts :: Opts
                    }

newtype Repl a = Repl { runRepl :: StateT ReplState (CLI.InputT IO) a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadState ReplState)

getInputLine :: String -> Repl (Maybe String)
getInputLine = Repl . lift . CLI.getInputLine

repl :: IO ()
repl = do
    env <- primitiveBindings
    putStrLn "loaded: stdlib.thm"
    CLI.runInputT CLI.defaultSettings $ evalStateT (runRepl replLoop) $ RS env Map.empty

replLoop :: Repl ()
replLoop = until_
    (isTerminationError . fst)
    (getInputLine ">>> " >>= replStep . fromMaybe "")
    (liftIO . putStrLn . showResult)

replStep :: String -> Repl (Either LispErr LispVal, EvalState)
replStep input = do
    replState <- get
    let cEnv = env replState
        cOpts = replOpts replState
        parse = labeledReadExpr input
    result@(_, evalState) <- liftIO (evaluate "<interactive>" cEnv cOpts input)
    put $ replState { replOpts = options evalState }
    return result

until_ :: Monad m
       => (a -> Bool) -- ^ predicate executed on the result of action
       -> m a         -- ^ monadic action which evaluates to the argument for action
       -> (a -> m ()) -- ^ function to call if pred succeeds
       -> m ()        -- ^ Compound action which sequences (prompt >>= action) until pred fails.
until_ pred prompt action = do
    result <- prompt
    unless (pred result)
        $ action result >> until_ pred prompt action
