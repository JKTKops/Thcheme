module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Control.Monad.Except (liftEither)

import           Types
import qualified Primitives.Bool             as BoolOps
import qualified Primitives.Char             as CharOps
import qualified Primitives.Comparison       as Comparison
import qualified Primitives.Misc             as Misc
import qualified Primitives.List             as List
import qualified Primitives.Math             as Math
import qualified Primitives.String           as String
import qualified Primitives.TypeCheck        as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast
import qualified Primitives.Vector           as Vector
import qualified Primitives.IOPrimitives     as IO

primitives :: HashMap String LispVal
primitives = foldr1 Map.union
                [ Map.mapWithKey makeRPrimitive rawPrimitives
                , Map.mapWithKey makeIPrimitive ioPrimitives
                , Map.mapWithKey makeEPrimitive ePrimitives
                , Map.mapWithKey makeMacroPrimitive macros
                ]

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
                            Misc.rawPrimitives ++
                            [("quit", quit)]

ioPrimitives :: HashMap String IOPrimitive
ioPrimitives = Map.fromList IO.ioPrimitives

ePrimitives :: HashMap String Primitive
ePrimitives = Map.fromList Misc.ePrimitives

macros :: HashMap String Macro
macros = Map.fromList $ String.macros ++
                        Vector.macros ++
                        List.macros ++
                        Misc.macros

makePrimitive :: String -> Arity -> Builtin -> LispVal
makePrimitive name arity f = Primitive arity f name

makeRPrimitive :: String -> RawPrimitive -> LispVal
makeRPrimitive name (RPrim arity func) =
    makePrimitive name arity (liftEither . func)

makeIPrimitive :: String -> IOPrimitive -> LispVal
makeIPrimitive name (IPrim arity func) =
    makePrimitive name arity (liftIOThrows . func)

makeEPrimitive :: String -> Primitive -> LispVal
makeEPrimitive name (Prim arity func) =
    makePrimitive name arity func

makeMacroPrimitive :: String -> Macro -> LispVal
makeMacroPrimitive name (Macro arity func) = PMacro arity func name

quit :: RawPrimitive
quit = RPrim 0 $ \_ -> Left Quit
