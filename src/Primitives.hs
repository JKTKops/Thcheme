-- | Collects and exports all of the Scheme primitives defined in Haskell.
--
-- Thcheme has a few non-Scheme primitives such as ex:repl, but ex:repl
-- and its friends are loaded at startup time by Bootstrap.
module Primitives (primitives) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import qualified Val
import qualified Primitives.Bool             as BoolOps
import qualified Primitives.Char             as CharOps
import qualified Primitives.Comparison       as Comparison
import qualified Primitives.Error            as Error
import qualified Primitives.Misc             as Misc
import qualified Primitives.List             as List
import qualified Primitives.Math             as Math
import qualified Primitives.String           as String
import qualified Primitives.Time             as Time
import qualified Primitives.TypeCheck        as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast
import qualified Primitives.Vector           as Vector
import qualified Primitives.IOPrimitives     as IO
import qualified Expander.SyntaxRules        as SyntaxRules

-- temporary
import Types

-- | Mapping from names of Thcheme primitives to their initial bindings.
-- Names are the names of primitives exported from Primitives.* modules,
-- in either Primitives.**.primitives or Primitives.**.macros.
primitives :: HashMap Symbol Val
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
      , Error.primitives
      , List.primitives
      , String.primitives
      , Time.primitives
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

macros :: HashMap Symbol Macro
macros = Map.fromList $ SyntaxRules.macros ++ Misc.macros

makeMacroPrimitive :: Symbol -> Macro -> Val
makeMacroPrimitive name (Macro arity func) = PrimMacro arity func name
