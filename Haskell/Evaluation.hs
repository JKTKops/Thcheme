-- The default case of Lithp is lower case.
{-# LANGUAGE ExistentialQuantification #-}
module Evaluation (eval) where

import Data.Maybe (isNothing)
import Data.IORef (IORef, readIORef, newIORef)
import Control.Monad.Except (ExceptT (ExceptT), liftIO, catchError, throwError)
import Control.Monad (liftM, mapM)
import qualified Data.Char as C 
    (ord, chr)
import qualified Data.HashMap.Strict as Map

import LispVal
import Environment

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Char _)   = return val
eval env val@(Number _) = return val
eval env val@(Bool _)   = return val
eval env (Atom id)      = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        _          -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom name : params) : body)) =
     makeFuncNormal env params body (Just name) >>= defineVar env name

eval env (List (Atom "define" : DottedList (Atom name : params) varargs : body)) =
     makeFuncVarargs varargs env params body (Just name) >>= defineVar env name

eval env (List (Atom "lambda" : List params : body)) =
     makeFuncNormal env params body Nothing

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeFuncVarargs varargs env params body Nothing

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeFuncVarargs varargs env [] body Nothing

eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (Primitive func _) args = liftThrows $ func args
apply (Func params varargs body closure _) args =
    if num params /= num args && isNothing varargs 
    then throwError $ NumArgs (num params) args
    else (liftIO . bindVars closure . Map.fromList $ zip params args)
            >>= bindVarargs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = last <$> mapM (eval env) body
          bindVarargs arg env = case arg of
            Just argName -> liftIO . bindVars env $ Map.fromList [(argName, List remainingArgs)]
            Nothing      -> return env
apply notFunc _ = throwError . NotFunction "Not a function" $ show notFunc

makeFunc :: Maybe String 
         -> Env 
         -> [LispVal] 
         -> [LispVal] 
         -> Maybe String 
         -> IOThrowsError LispVal
makeFunc varargs env params body name = 
    return $ Func (map show params) varargs body env name
    {-
    ExceptT $ do
        env' <- readIORef env
        deepCopy <- mapM copy env'
        newEnv <- newIORef deepCopy
        return . return $ Func (map show params) varargs body newEnv name
    where copy :: (String, IORef LispVal) -> IO (String, IORef LispVal)
          copy (name, ref) = do
            lv <- readIORef ref
            copy <- newIORef lv
            return (name, copy)
            -}

makeFuncNormal = makeFunc Nothing
makeFuncVarargs = makeFunc . Just . show
