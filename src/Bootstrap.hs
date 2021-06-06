module Bootstrap 
    ( primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
    
    , evaluate, evaluateExpr
    ) where

import Control.Monad (forM_, void)
import System.IO.Unsafe (unsafePerformIO)

import Paths_Thcheme
import Val
import EvaluationMonad (EvalState)
import Types.Port (pOpenInputFile)
import Parsers (labeledReadExprList, labeledReadExpr)
import Primitives (primitives)
import Environment (Env, bindVars, nullEnv, deepCopyEnv)
import Options (noOpts)
import qualified Evaluation

makePrimitiveBindings :: IO Env
makePrimitiveBindings = do
  ne <- nullEnv
  env <- bindVars ne primitives
  -- warning! This means the stdlib is evaluated with a different
  -- root dynamic point than the REPL. That /shouldn't/ cause any
  -- problems, but be aware if weird bugs are happening.
  s <- Evaluation.initEvalState env noOpts
  forM_ orderedBootstrapFiles $ \f ->
    bootstrapLoad f s
  bootstrapExec "(import (rnrs base))" s
  return env

primitiveEnv :: Env
primitiveEnv = unsafePerformIO makePrimitiveBindings
{-# NOINLINE primitiveEnv #-}

primitiveBindings :: IO Env
primitiveBindings = deepCopyEnv primitiveEnv

-- | Parse and evaluate a Scheme expression with the given initial state.
-- See 'evaluateExpr'.
evaluate :: String -> EvalState -> String -> IO (Either LispErr Val, EvalState)
-- Defined this way instead of parsing a wrapped 'src' string so that location
-- information is correct.
evaluate label s src = labeledReadExpr label src >>= \case
  Right v -> evaluateExpr s v
  Left e  -> pure (Left e, s)

-- | Evaluate a Scheme expression with the given initial state, for the
-- purposes of the REPL. The given state should represent a bootstrapped
-- environment such as 'primitiveBindings'.
-- 
-- The expression e is transformed into (ex:repl '(e)) and then handed off
-- to the primitive 'Evaluation.evaluateExpr' evaluator which does not know
-- anything in particular about the macro expander.
--
-- 'ex:repl' is defined by lib\/expander\/expander.scm, and it embeds a
-- macro system into a scheme that doesn't have one.
evaluateExpr :: EvalState -> Val -> IO (Either LispErr Val, EvalState)
evaluateExpr s e = Evaluation.evaluateExpr s callExpander
  where
    quoteE = makeImmutableList [Symbol "quote", makeImmutableList [e]]
    callExpander = makeImmutableList [Symbol "ex:repl", quoteE]

bootstrapRead :: FilePath -> IO (Either LispErr [Val])
bootstrapRead fname = do
  fpath <- getDataFileName fname
  port  <- pOpenInputFile $ pack fpath
  return $ labeledReadExprList fpath port

-- | Load a bootstrap scheme file from the data dir, read all
-- the forms in it, and then evaluate it in the given state.
-- The state should represent the (interaction-environment).
-- Forms are evaluated without the macro expander.
bootstrapLoad :: FilePath -> EvalState -> IO ()
bootstrapLoad fname s = do
  Right exprs <- bootstrapRead fname
  mapM_ (Evaluation.evaluateExpr s) exprs

-- | Execute a single given scheme expression and evaluate it
-- in the given state. Useful for the initial import.
bootstrapExec :: String -> EvalState -> IO ()
bootstrapExec src s = do
  Right expr <- labeledReadExpr "bootstrap" src
  void $ evaluateExpr s expr

stdlibName :: FilePath
stdlibName = "lib/stdlib.scm"

stdlib :: IO [Val]
stdlib = do
  Right exprs <- bootstrapRead stdlibName
  return exprs

orderedBootstrapFiles :: [FilePath]
orderedBootstrapFiles =
  [ stdlibName
  , "lib/records.scm"
  , "lib/expander/compat-thcheme.scm"
  , "lib/expander/runtime.scm"
  , "lib/expander/standard-libraries.exp"
  , "lib/expander/expander.exp"
  ]
