module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Control.Monad.Except (liftEither)

import           Types
import qualified Primitives.Bool             as BoolOps
import qualified Primitives.Char             as CharOps
import qualified Primitives.Comparison       as Comparison
import qualified Primitives.Functions        as Function
import qualified Primitives.List             as List
import qualified Primitives.Math             as Math
import qualified Primitives.String           as String
import qualified Primitives.TypeCheck        as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast
import qualified Primitives.Vector           as Vector
import qualified Primitives.IOPrimitives     as IO

primitives :: HashMap String LispVal
primitives = Map.union
                (Map.mapWithKey makeRPrimitive rawPrimitives)
                (Map.mapWithKey makeIPrimitive ioPrimitives)

rawPrimitives :: HashMap String RawPrimitive
rawPrimitives = Map.fromList $ Math.rawPrimitives ++
                            List.rawPrimitives ++
                            Vector.rawPrimitives ++
                            String.rawPrimitives ++
                            BoolOps.rawPrimitives ++
                            CharOps.rawPrimitives ++
                            Comparison.rawPrimitives ++
                            TypeCheck.rawPrimitives ++
                            TypeCast.rawPrimitives ++
                            Function.rawPrimitives ++
                            [("quit", quit)]

ioPrimitives :: HashMap String IOPrimitive
ioPrimitives = Map.fromList IO.ioPrimitives

makePrimitive :: (Arity -> a -> String -> LispVal) -> String -> Arity -> a -> LispVal
makePrimitive constructor name arity f = constructor arity f name

makeRPrimitive :: String -> RawPrimitive -> LispVal
makeRPrimitive name (RPrim arity func) =
    makePrimitive Primitive name arity (liftEither . func)

makeIPrimitive :: String -> IOPrimitive -> LispVal
makeIPrimitive name (IPrim arity func) =
    makePrimitive Primitive name arity (liftIOThrows . func)

quit :: RawPrimitive
quit = RPrim 0 $ \_ -> Left Quit
