{-# LANGUAGE TemplateHaskell #-}
module Bootstrap 
    (
      primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
    ) where

import Data.FileEmbed (embedStringFile)

import Types   ( ThrowsError, Val )
import Parsers (labeledReadExprList)

import Primitives (primitives)
import Environment (Env, bindVars, nullEnv)

import Options (noOpts)
import Evaluation (evaluateExpr)

primitiveBindings :: IO Env
primitiveBindings = do
    ne <- nullEnv
    env <- bindVars ne primitives
    let (Right exprs) = stdlib
    mapM_ (evaluateExpr env noOpts) exprs
    return env

-- we absolutely need to set up a data dir for "installing" packages
-- and do away with this embed file. It puts a _haskell string literal_
-- containing the entire contents of the file into the executable!

stdlib :: ThrowsError [Val]
stdlib = labeledReadExprList "stdlib" $(embedStringFile "src/stdlib.thm")
