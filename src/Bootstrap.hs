{-# LANGUAGE TemplateHaskell #-}
module Bootstrap (
      primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
    ) where

import Data.IORef (newIORef)
import Data.FileEmbed (embedStringFile)
import qualified Data.HashMap.Strict as Map (empty)

import Types
import Parsers (labeledReadExprList)

import Primitives (primitives)
import Environment (Env, bindVars, nullEnv)

import Evaluation (evaluateExpr)

primitiveBindings :: IO Env
primitiveBindings = do
    ne <- nullEnv
    env <- bindVars ne primitives
    let (Right exprs) = stdlib
    mapM_ (evaluateExpr env Map.empty) exprs
    return env

-- we absolutely need to set up a data dir for "installing" packages
-- and do away with this embed file. It puts a _haskell string literal_
-- containing the entire contents of the file into the executable!

stdlib :: ThrowsError [Val]
stdlib = labeledReadExprList "stdlib" $(embedStringFile "src/stdlib.thm")
