module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Control.Monad.Except (liftEither)

import           LispVal                     as Val
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

-- temporary
import Types

primitives :: HashMap String LispVal
primitives = foldr1 Map.union
                [ primitivesMap
                , Map.mapWithKey makeRPrimitive rawPrimitives
                , Map.mapWithKey makeMacroPrimitive macros
                ]
  where
    primitivesList = concat
      [ Math.primitives
      , BoolOps.primitives
      , CharOps.primitives
      , Comparison.primitives
      , TypeCheck.primitives
      , TypeCast.primitives
      , Misc.primitives
      , IO.primitives
      ]

    primitivesMap = Map.fromList
      [ (name, Val.makePrimitive prim)
      | prim@(Prim name _ _) <- primitivesList
      ]

rawPrimitives :: HashMap String RawPrimitive
rawPrimitives = Map.fromList $ List.rawPrimitives ++
                            Vector.rawPrimitives ++
                            String.rawPrimitives

macros :: HashMap String Macro
macros = Map.fromList $ String.macros ++
                        Vector.macros ++
                        List.macros ++
                        Misc.macros

makePrimitive :: String -> Arity -> Builtin -> LispVal
makePrimitive name arity f = Primitive arity f name

makeRPrimitive :: String -> RawPrimitive -> LispVal
makeRPrimitive name (RPrim arity func) =
    Primitives.makePrimitive name arity (liftEither . func)

makeMacroPrimitive :: String -> Macro -> LispVal
makeMacroPrimitive name (Macro arity func) = PMacro arity func name
