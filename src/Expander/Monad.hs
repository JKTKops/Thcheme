module Expander.Monad
  ( Expand, runExpand
  , scopeCounter, freshCounter

  , MonadReader (..), asks, lift
  , module EvaluationMonad
  ) where

import Control.Monad.Reader
import Data.IORef (newIORef)

import EvaluationMonad (EM, Ref, readRef, writeRef, modifyRef')

type Expand = ReaderT ExpandEnv EM

runExpand :: Expand a -> EM a
runExpand m = do
  env <- liftIO initExpandEnv
  runReaderT m env

data ExpandEnv = ExpandEnv
  { scopeCounter :: Ref Int
  , freshCounter :: Ref Int
  }

initExpandEnv :: IO ExpandEnv
initExpandEnv = ExpandEnv <$> newIORef 0 <*> newIORef 0
