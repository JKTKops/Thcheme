{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Repl where

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (isPrefixOf)

import qualified System.Console.Haskeline as CLI
import System.IO (hFlush, stdout)

import Paths_Thcheme (version)
import Data.Version (showVersion)

import Val
import EvaluationMonad ( EvalState (..), initEvalState, resetEvalState
                       , noOpts
                       )
import Evaluation ( showResultIO ) --, evaluate )
import Bootstrap ( primitiveBindings, evaluate )
import qualified Environment as Env (keys)

data ReplState = RS { replEvalState :: EvalState }

newtype Repl a = Repl { runRepl :: CLI.InputT (StateT ReplState IO) a }
  deriving (Monad, Functor, Applicative, MonadIO, MonadState ReplState)

repl :: IO ()
repl = do
    putStrLn $ "thcheme " ++ showVersion version
    env <- primitiveBindings
    estate <- initEvalState env noOpts
    replLoop
      & runRepl
      & CLI.runInputT settings
      & flip evalStateT (RS estate)

replLoop :: Repl ()
replLoop = until_
    (stop . fst)
    (getInput >>= replEval)
    (Repl . display)
  where stop Right{}  = False
        stop (Left e) = isTerminationError e

        display result = do
          -- if we don't hFlush, it seems like 'outputStrLn' misbehaves.
          -- e.g.
          -- >>> (write 5)
          -- >>> #t
          -- 5>>>
          liftIO $ hFlush stdout
          str <- liftIO $ showResultIO result
          case str of
            "" -> pure ()
            _  -> CLI.outputStrLn str

getInputLine :: String -> Repl (Maybe String)
getInputLine = Repl . CLI.getInputLine

getInput :: Repl (Maybe String)
getInput = runMaybeT $ go id 0
  where
    -- this is a bit optimized to avoid the awful behavior of
    -- ++ and also to avoid traversing the whole string every iteration
    -- to determine the balance.
    --
    -- On the other hand, it's also wrong. 'bracketBalance' doesn't do
    -- the right thing when it encounters brackets in comments or strings
    -- (excluding datum comments). The right thing to do is probably
    -- just to actually try parsing the input on each iteration. Hopefully
    -- no one is crazy enough to enter hundreds of lines of input at once
    -- and get bad quadratic behavior.
    --
    -- The really-right thing to do would be to have a reader that integrates
    -- with the parser, able to ask the parser if it has accepted or if it
    -- needs more input. I suspect this is possible using something like a
    -- coroutine monad.
    go :: (String -> String) -> Int -> MaybeT Repl String
    go prevLines balance = do 
        line <- MaybeT $ getInputLine prompt
        if balance == 0 && all isSpace line
        then go prevLines balance
        else do let inp = prevLines . showString line
                case balance + bracketBalance line of
                  -- add the newline here, rather than to the beginning of
                  -- line, so that parse errors give the right line number
                  -- 3/16/21
                  n | n > 0     -> go (inp . ('\n':)) n
                    | otherwise -> return $ inp ""
      where prompt = case balance of
       -- balance is only 0 on the first input, otherwise we
       -- would've returned the input after the last round.
       -- If the user enters only whitespace and hits enter,
       -- we loop around and should get 0 again.
              0 -> ">>> "
              _ -> "... "

replEval :: Maybe String -> Repl (Either LispErr Val, EvalState)
replEval Nothing = pure (Left Quit, error "state forced after CTRL-D, report a bug")
replEval (Just inp) = Repl $
    CLI.handleInterrupt (pure interrupted) $ 
    CLI.withInterrupt $ 
    runRepl (evaluateTotalInput inp)
  where
    interrupted = ( Right (Symbol "interrupted.")
                  , error "state forced after interrupt, report a bug"
                  )

evaluateTotalInput :: String -> Repl (Either LispErr Val, EvalState)
evaluateTotalInput input = do
    replState <- get
    let RS s = replState
    result@(_, evalState) <- liftIO (evaluate "<interactive>" s input)
    put $ RS $ resetEvalState evalState
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
  env <- gets (globalEnv . replEvalState)
  keys <- liftIO $ Env.keys env
  let candidates = filter (str `isPrefixOf`) $ map symbolAsString keys
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
