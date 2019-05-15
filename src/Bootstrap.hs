{-# LANGUAGE TemplateHaskell #-}
module Bootstrap (
      nullEnv
    , primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
    ) where

import Data.IORef (newIORef)
import Data.FileEmbed (embedStringFile)
import qualified Data.HashMap.Strict as Map (empty)

import Types
import Parsers (labeledReadExprList)

import Primitives (primitives)
import Environment (Env, bindVars)

import Evaluation (evaluateExpr)

nullEnv :: IO Env
nullEnv = newIORef Map.empty

primitiveBindings :: IO Env
primitiveBindings = do
    ne <- nullEnv
    env <- bindVars ne primitives
    let (Right exprs) = stdlib
    mapM_ (evaluateExpr env Map.empty) exprs
    return env

stdlib :: ThrowsError [LispVal]
stdlib = labeledReadExprList "stdlib" $(embedStringFile "src/stdlib.thm")
