{-# LANGUAGE LambdaCase #-}
module Primitives.Functions (primitives) where

import Control.Monad.Except (throwError)

import LispVal

primitives :: [(String, RawPrimitive)]
primitives = [ ("id", identityFunction) ]

identityFunction :: RawPrimitive
identityFunction = RPrim 1 $ \case
    [arg]   -> return arg
    badArgs -> throwError $ NumArgs 1 badArgs

-- compose?
