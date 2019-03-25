module Primitives (primitives) where

import LispVal (LispVal, ThrowsError, RawPrimitive)
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
             [ ("symbol?", TypeCheck.isSymbol)
             , ("string?", TypeCheck.isString)
             , ("char?", TypeCheck.isChar)
             , ("number?", TypeCheck.isNumber)
             , ("boolean?", TypeCheck.isBool)
             , ("list?", TypeCheck.isList)
             , ("pair?", TypeCheck.isPair)
             , ("&&", BoolOps.boolAnd)
             , ("||", BoolOps.boolOr)
             , ("not", BoolOps.boolNot)
             , ("=", Comparison.numeq)
             , ("<", Comparison.numlt)
             , (">", Comparison.numgt)
             , ("/=", Comparison.numne)
             , (">=", Comparison.numge)
             , ("<=", Comparison.numle)
             , ("string=?", Comparison.streq)
             , ("string<?", Comparison.strlt)
             , ("string>?", Comparison.strgt)
             , ("string<=?", Comparison.strle)
             , ("string>=?", Comparison.strge)
             , ("char=?", Comparison.chareq)
             , ("char<?", Comparison.charlt)
             , ("char>?", Comparison.chargt)
             , ("char<=?", Comparison.charle)
             , ("char>=?", Comparison.charge)
             , ("eq?", Comparison.eqv)
             , ("eqv?", Comparison.eqv)
             , ("equal?", Comparison.equal)
             , ("char-upcase", CharOps.toUpper)
             , ("char-downcase", CharOps.toLower)
             , ("char-alphabetic?", CharOps.isAlpha)
             , ("char-numeric?", CharOps.isNumber)
             , ("char-whitespace?", CharOps.isSpace)
             , ("char-upper-case?", CharOps.isUpper)
             , ("char-lower-case?", CharOps.isLower)
             , ("char->number", TypeCast.charToNumber)
             , ("char->string", TypeCast.charToString)
             , ("list->string", TypeCast.listToString)
             , ("number->char", TypeCast.numberToChar)
             , ("number->string", TypeCast.numberToString)
             , ("string->list", TypeCast.stringToList)
             , ("string->number", TypeCast.stringToNumber)
             ]
