module Primitives (primitives) where

import LispVal (LispVal, LispErr (Quit), ThrowsError, RawPrimitive)
import qualified Primitives.Math as Math
import qualified Primitives.List as List
import qualified Primitives.Bool as BoolOps
import qualified Primitives.Char as CharOps
import qualified Primitives.Comparison as Comparison
import qualified Primitives.TypeCheck as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast

-- TODO replace list structure with a HashMap String (RawPrimitive)
-- from Data.HashMap.Strict
-- Safe to assume we need all functions so laziness is wasted overhead
primitives :: [(String, RawPrimitive)]
primitives = Math.primitives ++
             List.primitives ++
             BoolOps.primitives ++
             CharOps.primitives ++
             Comparison.primitives ++
             TypeCheck.primitives ++
             TypeCast.primitives ++
             [("quit", quit)]

quit :: [LispVal] -> ThrowsError LispVal
quit _ = Left Quit
