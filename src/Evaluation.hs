module Evaluation
    ( -- | evaluate a string
      evaluate
      -- | evaluate a given expression
    , evaluateExpr
      -- | evaluate inside EM monad
    , eval
    , runTest
      -- | Convert the evaluation output into a meaningful string
    , showResultIO
      -- | Function application, not sure why this is here rn
    , apply
    ) where

import Data.Maybe
import Control.Monad
import qualified Data.HashMap.Strict as Map

import Parsers
import Val
import EvaluationMonad
import qualified Environment as Env
import Options (checkOpt, Opt(FullStackTrace))
import Primitives.WriteLib (writeSharedSH, showErrIO)

eval :: Val -> EM Val
eval v = do
    pushExpr Call v
    res <- evalExpr v
    popExpr
    return res

evalExpr :: Val -> EM Val
evalExpr expr = do
    fexpr <- freezeList expr
    case fexpr of
        FList (function : args) -> handleApp expr function args
        FList [] -> return Nil -- see Note: [Freezing Nil] in Val.hs
        FNotList obj -> handleSimpleDatum obj
        FDottedList{} ->
            throwError $ BadSpecialForm expr

handleSimpleDatum :: Val -> EM Val
handleSimpleDatum obj = case obj of
    val@String{} -> return val
    val@Char{}   -> return val
    val@Number{} -> return val
    val@Bool{}   -> return val
    val@Vector{} -> return val
    (Atom id)    -> getVar id
    Nil          -> return Nil
    _other -> panic "handleSimpleDatum: datum is not simple!"

handleApp :: Val -- ^ the original unfrozen Val, to put on the stack
          -> Val -> [Val] -> EM Val
handleApp form function args = do
    func <- case function of
        Primitive{}    -> return function
        Continuation{} -> return function
        Closure{}      -> return function
        PrimMacro{}    -> return function
        _              -> eval function
    case func of
        Primitive{}    -> evalCall func
        Continuation{} -> evalCall func
        Closure{}      -> evalCall func
        PrimMacro{}    -> evalPMacro func
        -- TODO: this doesn't play well with the introduction of mutable
        -- lists, so it really is time to make a proper distinction between
        -- functions and low-level macros.
        p@Pair{} -> do
            fp <- freezeList p
            case fp of
                FDottedList [Atom "macro"] macro@Closure{} -> evalMacro macro
                _ -> evalCall func
        _              -> evalCall func

  where evalCall func = do
            argVals <- mapM eval args
            let reduced = func `isReduced` function || args /= argVals
            when reduced $ do
                modifyTopReason $ const Reduce
                pushExpr Call form
            v <- apply func argVals
            when reduced popExpr
            return v
        evalPMacro pmacro = do
            modifyTopReason $ const Expand
            apply pmacro args
        evalMacro macro = do
            modifyTopReason $ const Expand
            expansion <- apply macro args
            eval expansion

        isReduced :: Val -> Val -> Bool
        isReduced _ (Atom _) = False
        isReduced new old    = new /= old

apply :: Val -> [Val] -> EM Val

-- Applications of primitive functions
apply (Primitive arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

apply (PrimMacro arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

-- Application of continuation
apply (Continuation func) [arg] = func arg
apply Continuation{} badArgs = throwError $ NumArgs 1 badArgs

-- Applications of user-defined functions
apply (Closure params varargs body cloEnv _name) args =
    case num args `compare` num params of
        -- Throw error if too many args and no varargs
        GT -> if isNothing varargs
              then throwError $ NumArgs (num params) args
        -- otherwise bind parameters in this closure, bind the varargs, and evaluate
              else evaluate
        EQ -> evaluate
        LT -> throwError $ NumArgs (num params) args

  where evaluate = do env <- makeBindings params
                      env' <- bindVarargs varargs env
                      pushEnv env'
                      -- if we just use the captured scope, local defines will
                      -- see the outer-defined IORefs and overwrite them, so
                      -- we need to layer a fresh scope on top of the captured
                      -- one.
                      result <- withNewScope evalBody
                      popEnv
                      return result

        makeBindings :: [String] -> EM Env
        makeBindings vars = liftIO . Env.bindVars cloEnv . Map.fromList $ zip vars args

        remainingArgs = drop (length params) args
        -- evaluate every expression in body, return value of the last one
        evalBody = last <$> mapM eval body
        -- binds extra arguments to vararg in GT case
        bindVarargs arg env = case arg of
          Just argName -> do
              remainingArgsMutable <- makeMutableList remainingArgs
              liftIO . Env.bindVars env $ Map.fromList [(argName, remainingArgsMutable)]
          Nothing -> return env

apply notFunc _ = throwError $ NotFunction "Not a function" notFunc

num :: [a] -> Integer
num = toInteger . length

evaluate :: String -> Env -> Opts -> String -> IO (Either LispErr Val, EvalState)
evaluate label initEnv opts input = execEM initEnv opts $
  liftEither (labeledReadExpr label input) >>= eval

evaluateExpr :: Env -> Opts -> Val -> IO (Either LispErr Val, EvalState)
evaluateExpr env opts v = execEM env opts $ eval v

-- | Provided for backwards compatibility.
runTest :: Env -> Opts -> EM Val -> IO (Either LispErr Val, EvalState)
runTest = execEM

showResultIO :: (Either LispErr Val, EvalState) -> IO String
showResultIO res = case res of
    (Left e@(Parser _), _) -> pure $ show e ++ "\n"
    (Left err, s) -> diffLines <$> showErrIO err <*> showEvalState s
    (Right v, _)  -> writeSharedSH v
  where diffLines s r = s ++ "\n" ++ r

-- I'd rather this was in EvaluationMonad, but this is almost as good.
-- In all honestly, there's a real argument that it belongs in Repl.
-- If Thcheme ever becomes a compiler, this does need to be bundled with
-- main to display any errors that the program raises and doesn't catch.
-- If that ever happens, then it stops really mattering, because all of
-- Thcheme would need to be in the Thcheme library, right? Maybe that's not
-- right. Hmm.
data TraceType = CallOnly | FullHistory deriving (Eq, Show, Read, Enum)
showEvalState :: EvalState -> IO String
showEvalState es = ("Stack trace:\n" ++) <$> numberedLines
  where numberedLines :: IO String
        -- unlines puts an extra newline at the end, which we
        -- actually want because it looks better.
        numberedLines = unlines . zipWith (<+>) numbers <$> exprs
        numbers = map (\i -> show i ++ ";") [1..]

        fstOpt :: TraceType
        fstOpt = if checkOpt FullStackTrace (options es)
                 then FullHistory
                 else CallOnly

        -- Note that we use write-shared here. It's possible that the error
        -- was raised because we tried to 'eval' a cyclic list. If we don't
        -- use write-shared, that will make the program hang.
        --
        -- Optimally, we would probably get information from showResultIO
        -- about whether or not we need to worry about that; the error itself
        -- is always displayed using write-shared for values it contains.
        -- However, we still need to be careful! Trying to evaluate cyclic
        -- _data_ won't crash (in fact, the program it represents could even
        -- be a correct, terminating program). Attempting to write-simple
        -- cyclic data will still hang, and write-shared is actually more
        -- efficient than write!
        exprs = if fstOpt == CallOnly
                then mapM (writeSharedSH . snd) 
                      . filter ((`elem` [Call, Expand]) . fst) 
                      $ stack es
                else mapM (\(s, v) ->
                         let buffer = case s of
                                 Call -> "    "
                                 Reduce -> "  "
                                 Expand -> "  "
                         in ((show s ++ ":" ++ buffer) ++)
                            <$> writeSharedSH v)
                     $ stack es

(<+>) :: String -> String -> String
"" <+> s  = s
s  <+> "" = s
s1 <+> s2 = s1 ++ " " ++ s2
