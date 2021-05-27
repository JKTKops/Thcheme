module Bootstrap 
    ( primitiveBindings
    , stdlib -- for testing... might want to move to Bootstrap.Internal
    ) where

import Control.Monad (forM_)

import Paths_Thcheme
import Types (LispErr, Val, EvalState)
import Parsers (labeledReadExprList)
import Primitives (primitives)
import Environment (Env, bindVars, nullEnv)
import Options (noOpts)
import Evaluation (evaluateExpr, initEvalState)

-- TODO: this currently re-constructs the environment from scratch,
-- i.e., _reloads all the bootstrap files_, every time it is called.
-- We'd rather produce it once and then make copies, which will
-- require some use of unsafePerformIO.
-- This issue makes the tests slow, so it should be fixed
-- sooner rather than later.
primitiveBindings :: IO Env
primitiveBindings = do
  ne <- nullEnv
  env <- bindVars ne primitives
  -- warning! This means the stdlib is evaluated with a different
  -- root dynamic point than the REPL. That /shouldn't/ cause any
  -- problems, but be aware if weird bugs are happening.
  s <- initEvalState env noOpts
  forM_ orderedBootstrapFiles $ \f ->
    bootstrapLoad f s
  return env

bootstrapRead :: FilePath -> IO (Either LispErr [Val])
bootstrapRead fname = do
  fpath <- getDataFileName fname
  fsrc  <- readFile fpath
  return $ labeledReadExprList fpath fsrc

-- | Load a bootstrap scheme file from the data dir, read all
-- the forms in it, and then evaluate it in the given state.
-- The state should represent the (interaction-environment).
bootstrapLoad :: FilePath -> EvalState -> IO ()
bootstrapLoad fname s = do
  Right exprs <- bootstrapRead fname
  mapM_ (evaluateExpr s) exprs

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
  ]
