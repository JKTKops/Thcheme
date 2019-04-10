module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import LispVal (LispVal, LispErr (Quit), ThrowsError, RawPrimitive)
import qualified Primitives.Math as Math
import qualified Primitives.List as List
import qualified Primitives.Bool as BoolOps
import qualified Primitives.Char as CharOps
import qualified Primitives.Comparison as Comparison
import qualified Primitives.TypeCheck as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast

primitives :: HashMap String RawPrimitive
primitives = Map.fromList $ Math.primitives ++
                            List.primitives ++
                            BoolOps.primitives ++
                            CharOps.primitives ++
                            Comparison.primitives ++
                            TypeCheck.primitives ++
                            TypeCast.primitives ++
                            [("quit", quit)]

quit :: [LispVal] -> ThrowsError LispVal
quit _ = Left Quit
