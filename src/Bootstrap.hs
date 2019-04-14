module Bootstrap (
      nullEnv
    , primitiveBindings
    ) where

import Data.IORef
import qualified Data.HashMap.Strict as Map

import Primitives (primitives)
import Environment (Env, bindVars)

nullEnv :: IO Env
nullEnv = newIORef Map.empty

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars primitives
