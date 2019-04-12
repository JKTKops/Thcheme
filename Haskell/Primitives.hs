module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import LispVal (LispVal(Primitive, IOPrimitive), LispErr (Quit)
               , ThrowsError, RawPrimitive, IOPrimitive)
import qualified Primitives.Math as Math
import qualified Primitives.List as List
import qualified Primitives.Bool as BoolOps
import qualified Primitives.Char as CharOps
import qualified Primitives.Comparison as Comparison
import qualified Primitives.TypeCheck as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast
import qualified Primitives.Functions as Function
import qualified Primitives.IOPrimitives as IO

primitives :: HashMap String LispVal
primitives = Map.union 
                (Map.mapWithKey makeRPrimitive rawPrimitives) 
                (Map.mapWithKey makeIPrimitive ioPrimitives)

rawPrimitives :: HashMap String RawPrimitive
rawPrimitives = Map.fromList $ Math.primitives ++
                            List.primitives ++
                            BoolOps.primitives ++
                            CharOps.primitives ++
                            Comparison.primitives ++
                            TypeCheck.primitives ++
                            TypeCast.primitives ++
                            Function.primitives ++
                            [("quit", quit)]

ioPrimitives :: HashMap String IOPrimitive
ioPrimitives = Map.fromList IO.primitives

makePrimitive :: (a -> String -> LispVal) -> String -> a -> LispVal
makePrimitive constructor name f = constructor f name

makeRPrimitive :: String -> RawPrimitive -> LispVal
makeRPrimitive = makePrimitive Primitive

makeIPrimitive :: String -> IOPrimitive -> LispVal
makeIPrimitive = makePrimitive IOPrimitive

quit :: [LispVal] -> ThrowsError LispVal
quit _ = Left Quit
