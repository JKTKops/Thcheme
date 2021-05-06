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
import Evaluation (evaluateExpr, initEvalState)

primitiveBindings :: IO Env
primitiveBindings = do
    ne <- nullEnv
    env <- bindVars ne primitives
    -- warning! This means the stdlib is evaluated with a different
    -- root dynamic point than the REPL. That /shouldn't/ cause any
    -- problems, but be aware if weird bugs are happening.
    s <- initEvalState env noOpts
    let (Right exprs) = stdlib
    mapM_ (evaluateExpr s) exprs
    return env

-- we absolutely need to set up a data dir for "installing" packages
-- and do away with this embed file. It puts a _haskell string literal_
-- containing the entire contents of the file into the executable!

stdlib :: ThrowsError [Val]
stdlib = labeledReadExprList "stdlib" $(embedStringFile "src/stdlib.thm")
