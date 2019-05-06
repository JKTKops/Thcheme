module Evaluation
    ( evaluate -- ^ evaluate a string
    , evaluateExpr -- ^ evaluate a given expression
    , runTest
    , showResult -- ^ Convert the evaluation output into a meaningful string
    , apply -- ^ Function application, not sure why this is here rn
    ) where

import Data.Maybe
import Data.Either
import Data.IORef
import Data.Array
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Char as C (ord, chr)
import qualified Data.HashMap.Strict as Map

import Parsers
import Types
import qualified Environment as Env

eval :: LispVal -> EM LispVal
eval v = do
    pushExpr Call v
    res <- evalExpr v
    popExpr
    return res

-- TODO AFTER adding macros -> refactor the below into primitive macros
evalExpr :: LispVal -> EM LispVal
evalExpr expr = case expr of
    val@(String _) -> return val
    val@(Char _)   -> return val
    val@(Number _) -> return val
    val@(Bool _)   -> return val
    val@(Vector _) -> return val
    (Atom id)      -> getVar id
    nil@(List [])  -> return nil
    List (arg0@(Atom fName) : argExprs) ->
        let fPrim = case fName of
              "quote"       -> primQuote
              "if"          -> primIf
              "set!"        -> primSet
              "string-set!" -> primStringSet
              "vector-set!" -> primVectorSet
              "set-option!" -> primSetOpt
              "define"      -> primDefine
              "lambda"      -> primLambda
              "begin"       -> primBegin
              "eval"        -> primEval
              "apply"       -> primApply
              "load"        -> primLoad
              _             -> handleNonPrim arg0
        in fPrim argExprs
    List (function : args) -> handleNonPrim function args
    badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

-- TODO lift all primitives into EM and then as appropriate put these under primitives
primQuote :: [LispVal] -> EM LispVal
primQuote [v]     = return v
primQuote badArgs = throwError $ NumArgs 1 badArgs


primIf :: [LispVal] -> EM LispVal
primIf (pred : conseq : alts) = do
    p <- eval pred
    if truthy p
    then eval conseq
    else case alts of
        [] -> return $ List []
        xs -> last <$> mapM eval xs
primIf badArgs = throwError $ Default $ "Expected at least 2 args; found " ++ show badArgs


primSet :: [LispVal] -> EM LispVal
primSet [Atom var, form] = eval form >>= setVar var
primSet [notAtom, _] = throwError $ TypeMismatch "symbol" notAtom
primSet badArgs = throwError $ NumArgs 2 badArgs

primStringSet :: [LispVal] -> EM LispVal
primStringSet args@(Atom _ : _) = update stringSetHelper args
primStringSet args = stringSetHelper args

stringSetHelper :: [LispVal] -> EM LispVal
stringSetHelper args = case args of
    [String s, Number i, Char c] -> case fromIntegral i of
        n | n `elem` [0..length s - 1] -> let (pre, (_:post)) = splitAt n s in
               return . String $ pre ++ (c:post)
          | otherwise -> throwError . Default $ "String index out of bounds: " ++ show n
    [String _, Number _, notChr] -> throwError $ TypeMismatch "char" notChr
    [String _, notNum, _] -> throwError $ TypeMismatch "number" notNum
    [notStr, _, _] -> throwError $ TypeMismatch "string" notStr
    badArgs -> throwError $ NumArgs 3 badArgs

primVectorSet :: [LispVal] -> EM LispVal
primVectorSet args@(Atom _ : _) = update vectorSetHelper args
primVectorSet args = vectorSetHelper args

vectorSetHelper :: [LispVal] -> EM LispVal
vectorSetHelper args = case args of
    [Vector arr, Number i, val]
        | i `elem` [0.. snd (bounds arr)] ->
              return . Vector $ arr // [(i, val)]
        | otherwise -> throwError . Default $ "Vector index out of bounds: " ++ show i
    [Vector _, notNum, _] -> throwError $ TypeMismatch "number" notNum
    [notVec, _, _] -> throwError $ TypeMismatch "vector" notVec
    badArgs -> throwError $ NumArgs 3 badArgs

-- Assumes the first element of args is an Atom; finds it and updates it with given func
update :: ([LispVal] -> EM LispVal) -> [LispVal] -> EM LispVal
update updater (Atom var : rest) = do
    envRef <- search var
    when (isNothing envRef) $ throwError $ UnboundVar "[Set] unbound symbol" var
    env <- liftIO . readIORef $ fromJust envRef
    let ref = fromJust $ Map.lookup var env
    val <- liftIO $ readIORef ref
    updated <- updater (val : rest)
    liftIO $ writeIORef ref updated
    return updated

primSetOpt :: [LispVal] -> EM LispVal
primSetOpt [Atom optName, form] = do
    val <- eval form
    state <- get
    let opts = options state
        opts' = Map.insert optName val opts
    put $ state { options = opts' }
    return val
primSetOpt [notAtom, _] = throwError $ TypeMismatch "symbol" notAtom
primSetOpt badArgs = throwError $ NumArgs 2 badArgs


primDefine :: [LispVal] -> EM LispVal
primDefine [Atom var, form] = eval form >>= \val ->
    let renamed = case val of
            Func {} -> val { name = Just var }
            _ -> val
    in defineVar var renamed
primDefine (List (Atom name : params) : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncNormal params body (Just name) >>= defineVar name

primDefine (DottedList (Atom name : params) varargs : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs params body (Just name) >>= defineVar name

primDefine (List (notAtom : _) : _) = throwError $ TypeMismatch "symbol" notAtom
primDefine (DottedList (notAtom : _) _ : _) = throwError $ TypeMismatch "symbol" notAtom
primDefine (notAtomOrList : _) = throwError $ TypeMismatch "symbol or list" notAtomOrList
primDefine badArgs = throwError $ NumArgs 2 badArgs


primLambda :: [LispVal] -> EM LispVal
primLambda (List params : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncNormal params body Nothing
primLambda (DottedList params varargs : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs params body Nothing
primLambda (varargs@(Atom _) : body) = case body of
    [] -> throwError emptyBodyError
    _  -> makeFuncVarargs varargs [] body Nothing
primLambda (notAtomOrList : _) = throwError $ TypeMismatch "symbol or list" notAtomOrList
primLambda badArgs = throwError $ NumArgs 2 badArgs

emptyBodyError :: LispErr
emptyBodyError = Default "Attempt to define function with no body"


primBegin :: [LispVal] -> EM LispVal
primBegin []    = throwError $ Default "Expected at least 1 arg; found []"
primBegin stmts = last <$> mapM eval stmts


primEval :: [LispVal] -> EM LispVal
primEval [form]  = eval form
primEval badArgs = throwError $ NumArgs 1 badArgs

primApply :: [LispVal] -> EM LispVal
primApply [func, List args] = apply func args
primApply (func : args) = apply func args
primApply [] = throwError $ Default "Expected at least 1 arg; found []"

primLoad :: [LispVal] -> EM LispVal
primLoad [String filename] = do
    file <- liftIO . runExceptT $ load filename
    case file of
        Left e   -> throwError e
        Right ls -> last <$> mapM eval ls
primLoad [notString] = throwError $ TypeMismatch "string" notString
primLoad badArgs     = throwError $ NumArgs 1 badArgs


handleNonPrim :: LispVal -> [LispVal] -> EM LispVal
handleNonPrim function args = do
    func <- case function of
        Primitive {}   -> return function
        Func {}        -> return function
        _              -> eval function
    argVals <- mapM eval args
    let reduced = function /= func || args /= argVals
    when reduced $ do
        modifyTopReason $ const Reduce
        pushExpr Call (List (function : argVals))
    v <- apply func argVals
    when reduced popExpr
    return v


apply :: LispVal -> [LispVal] -> EM LispVal

-- Applications of primitive functions
apply (Primitive arity func _) args
   | num args >= arity = func args
   | otherwise         = throwError $ NumArgs arity args

-- Applications of user-defined functions
apply (Func params varargs body closure name) args =
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
                      result <- evalBody
                      popEnv
                      return result

        makeBindings :: [String] -> EM Env
        makeBindings vars = liftIO . Env.bindVars closure . Map.fromList $ zip vars args

        remainingArgs = drop (length params) args
        -- evaluate every expression in body, return value of the last one
        evalBody = last <$> mapM eval body
        -- binds extra arguments to vararg in GT case
        bindVarargs arg env = case arg of
          Just argName ->
              liftIO . Env.bindVars env $ Map.fromList [(argName, List remainingArgs)]
          Nothing -> return env

apply notFunc _ = throwError . NotFunction "Not a function" $ show notFunc

num :: [a] -> Integer
num = toInteger . length

makeFunc :: Maybe String
         -> [LispVal]
         -> [LispVal]
         -> Maybe String
         -> EM LispVal
makeFunc varargs params body name = do
    emptyEnv <- liftIO $ newIORef Map.empty
    return $ Func (map show params) varargs body emptyEnv name

makeFuncNormal = makeFunc Nothing
makeFuncVarargs = makeFunc . Just . show

truthy :: LispVal -> Bool
truthy v = not $ v == Bool False
              || v == Number 0
              || v == List []
              || v == String ""


showResult :: (Either LispErr LispVal, EvalState) -> String
showResult res = case res of
    (Left e@(Parser _), _) -> show e ++ "\n"
    (Left err, s) -> show err ++ "\n" ++ show s
    (Right v, _)  -> show v

evaluate :: Env -> Opts -> String -> IO (Either LispErr LispVal, EvalState)
evaluate initEnv opts input =
    flip runStateT (ES [] [initEnv] 0 opts) . runExceptT . runEM $ do
        v <- liftEither $ readExpr input
        eval v

evaluateExpr :: Env -> Opts -> LispVal -> IO (Either LispErr LispVal, EvalState)
evaluateExpr env opts v =
    flip runStateT (ES [] [env] 0 opts) . runExceptT . runEM $ eval v

runTest :: Env -> Opts -> EM LispVal -> IO (Either LispErr LispVal, EvalState)
runTest env opts m = flip runStateT (ES [] [env] 0 opts) . runExceptT $ runEM m

-- | Push an expr to the call stack
pushExpr :: StepReason -> LispVal -> EM ()
pushExpr r val = do
    s <- get
    put $ s { stack = (r, val) : stack s }

-- | Remove the top item from the call stack
popExpr :: EM ()
popExpr = modify $ \s -> s { stack = tail $ stack s }

-- | Push an environment to the top of the sEnv stack
--   The environment becomes the topmost scope of evaluation.
pushEnv :: Env -> EM ()
pushEnv e = modify $ \s -> s { symEnv = e : symEnv s }

-- | Remove the topmost scope from the sEnv stack.
popEnv :: EM ()
popEnv = do
    s <- get
    let stack = symEnv s
    put $ s { symEnv = tail stack }

modifyStackTop :: ((StepReason, LispVal) -> (StepReason, LispVal)) -> EM ()
modifyStackTop f = modify $ \s ->
    let top : tail = stack s in
    s { stack = f top : tail }

modifyTopReason :: (StepReason -> StepReason) -> EM ()
modifyTopReason f = modifyStackTop (\(r, v) -> (f r, v))

-- | Searches the environment stack top-down for a symbol
getVar :: String -> EM LispVal
getVar var = do
    stack <- symEnv <$> get
    l <- liftIO $ rights <$> mapM (\env -> runExceptT $ Env.getVar env var) stack
    case l of
        []  -> throwError $ UnboundVar "[Get] unbound symbol" var
        v:_ -> return v

-- | Search the environment top-down for a symbol
--   If it's found, bind it to the given LispVal
--   Otherwise, create a new binding in the top-level
setVar :: String -> LispVal -> EM LispVal
setVar var val = do
    mEnv <- search var
    case mEnv of
        Nothing -> defineVar var val
        Just e  -> do
            Right v <- liftIO . runExceptT $ Env.defineVar e var val
            return v

search :: String -> EM (Maybe Env)
search var = do
    stack <- symEnv <$> get
    EM . lift $ do
        l <- liftIO $ catMaybes <$> mapM
             (\env -> do
                   e <- runExceptT $ Env.getVar env var
                   return $ if isRight e then Just env else Nothing
             ) stack
        return $ case l of
            []  -> Nothing
            e:_ -> Just e

defineVar :: String -> LispVal -> EM LispVal
defineVar var val = do
    env <- head . symEnv <$> get
    liftIOThrows $ Env.defineVar env var val

quasiQuote :: EM ()
quasiQuote = modify $ \s -> s { quoteLevel = quoteLevel s + 1 }

unquote :: EM ()
unquote = do
    ql <- quoteLevel <$> get
    if ql /= 0
    then modify $ \s -> s { quoteLevel = ql - 1 }
    else throwError $ Default "Cannot unquote outside quasiquote"
