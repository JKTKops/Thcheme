{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Evaluation.Types
    ( EM (..)
    , EvalState (..)
    , StepReason (..)
    , Opts
    , TraceType (..)
    ) where

import Control.Monad.Fail
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.HashMap.Strict as Map

import LispVal

-- | The Evaluation Monad
type EMt = ExceptT LispErr (StateT EvalState IO)
newtype EM a = EM { runEM :: EMt a }
  deriving ( Monad, Functor, Applicative, MonadIO
           , MonadError LispErr, MonadState EvalState)

instance MonadFail EM where
    fail s = throwError . Default $ s ++ "\nAn error occurred, please report a bug."

-- | Reasons a step was performed
data StepReason = Call | Reduce deriving (Eq, Show, Read, Enum)
-- | Type of options from the REPL
type Opts = Map.HashMap String LispVal

-- | The current state of evaluation
data EvalState = ES { stack      :: [(StepReason, LispVal)]
                    , symEnv     :: [Env]
                    , quoteLevel :: Int
                    , options    :: Opts
                    }

data TraceType = CallOnly | FullHistory deriving (Eq, Show, Read, Enum)
