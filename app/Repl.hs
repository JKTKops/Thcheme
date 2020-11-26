{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Repl where

import System.IO
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as Map

import qualified System.Console.Haskeline as CLI

import Types
import Parsers
import Evaluation
import Bootstrap
import qualified Environment as Env (keys)

data ReplState = RS { env      :: Env
                    , replOpts :: Opts
                    }

newtype Repl a = Repl { runRepl :: CLI.InputT (StateT ReplState IO) a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadState ReplState)

repl :: IO ()
repl = do
    env <- primitiveBindings
    putStrLn "loaded: stdlib.thm"
    replLoop
      & runRepl
      & CLI.runInputT settings
      & flip evalStateT (RS env Map.empty)

replLoop :: Repl ()
replLoop = until_
    (isTerminationError . fst)
    (getInput >>= replEval)
    (Repl . CLI.outputStrLn . showResult)

getInputLine :: String -> Repl (Maybe String)
getInputLine = Repl . CLI.getInputLine

getInput :: Repl (Maybe String)
getInput = runMaybeT $ go id 0
  where
    -- this is a bit optimized to avoid the awful behavior of
    -- ++ and also to avoid traversing the whole string every iteration
    -- to determine the balance.
    go :: (String -> String) -> Int -> MaybeT Repl String
    go prevLines balance = do 
        line <- MaybeT $ getInputLine prompt
        if balance == 0 && all isSpace line
        then go prevLines balance
        else do let inp = prevLines . showString ('\n':line)
                case balance + bracketBalance line of
                  n | n > 0     -> go inp n
                    | otherwise -> return $ inp ""
      where prompt = case balance of
       -- balance is only 0 on the first input, otherwise we
       -- would've returned the input after the last round.
              0 -> ">>> "
              _ -> "... "

replEval :: Maybe String -> Repl (Either LispErr LispVal, EvalState)
replEval Nothing = pure (Left Quit, error "state forced after CTRL-D, report a bug")
replEval (Just inp) = Repl $
    CLI.handleInterrupt (pure interrupted) $ 
    CLI.withInterrupt $ 
    runRepl (evaluateTotalInput inp)
  where
    interrupted = ( Right (Atom "Interrupted.")
                  , error "state forced after interrupt, report a bug"
                  )

evaluateTotalInput :: String -> Repl (Either LispErr LispVal, EvalState)
evaluateTotalInput input = do
    replState <- get
    let cEnv = env replState
        cOpts = replOpts replState
    result@(_, evalState) <- liftIO (evaluate "<interactive>" cEnv cOpts input)
    -- since Env = IORef (HashMap...), executing the 'evaluate' action has already
    -- updated everything in the env in the replState!
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

bracketBalance :: String -> Int
bracketBalance = go 0
  where
    go n "" = n
    go n (c:rest)
      | c `elem` ['(', '{', '['] = go (n+1) rest
      | c `elem` [')', '}', ']'] = go (n-1) rest
      | otherwise = go n rest

type M = StateT ReplState IO
searchFunc :: String -> M [CLI.Completion]
searchFunc str = do
  env <- gets env
  keys <- liftIO $ Env.keys env
  let candidates = filter (str `isPrefixOf`) $ sort keys
      completions = map CLI.simpleCompletion candidates
  return completions

completionFunc :: CLI.CompletionFunc M
completionFunc = envCompleter `CLI.fallbackCompletion` CLI.completeFilename
  where envCompleter = CLI.completeWord Nothing " \t()" searchFunc

settings :: CLI.Settings M
settings = CLI.setComplete completionFunc CLI.defaultSettings

-- Haskeline should really provide this
instance MonadState ReplState (CLI.InputT M) where
    get   = lift get
    put   = lift . put
    state = lift . state
