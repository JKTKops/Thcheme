module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import qualified Val
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
import qualified Expander.SyntaxRules        as SyntaxRules

-- temporary
import Types

primitives :: HashMap String Val
primitives = foldr1 Map.union
                [ primitivesMap
                , Map.mapWithKey makeMacroPrimitive macros
                ]
  where
    primitivesList = concat
      [ Math.primitives
      , BoolOps.primitives
      , CharOps.primitives
      , Comparison.primitives
      , List.primitives
      , String.primitives
      , Vector.primitives
      , TypeCheck.primitives
      , TypeCast.primitives
      , Misc.primitives
      , IO.primitives
      ]

    primitivesMap = Map.fromList
      [ (name, Val.makePrimitive prim)
      | prim@(Prim name _ _) <- primitivesList
      ]

macros :: HashMap String Macro
macros = Map.fromList $ SyntaxRules.macros ++ Misc.macros

makeMacroPrimitive :: String -> Macro -> Val
makeMacroPrimitive name (Macro arity func) = PrimMacro arity func name
