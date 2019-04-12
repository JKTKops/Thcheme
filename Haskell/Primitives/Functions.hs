module Primitives.Functions (primitives) where

import Control.Monad.Except (throwError)

import LispVal

primitives :: [(String, RawPrimitive)]
primitives = [ ("id", identityFunction) ]

identityFunction :: RawPrimitive
identityFunction [arg] = return arg
identityFunction badArgs = throwError $ NumArgs 1 badArgs
