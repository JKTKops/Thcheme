{-# LANGUAGE LambdaCase #-}
module Primitives.Functions (rawPrimitives) where

import Control.Monad.Except (throwError)

import Types

rawPrimitives :: [(String, RawPrimitive)]
rawPrimitives = [ ("id", identityFunction) ]

identityFunction :: RawPrimitive
identityFunction = RPrim 1 $ \case
    [arg]   -> return arg
    badArgs -> throwError $ NumArgs 1 badArgs

-- compose?
