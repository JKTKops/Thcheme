module Primitives (primitives) where

import LispVal
import qualified Primitives.Math as Math
import qualified Primitives.List as List
import qualified Primitives.Bool as BoolOps
import qualified Primitives.Char as CharOps
import qualified Primitives.Comparison as Comparison
import qualified Primitives.TypeCheck as TypeCheck
import qualified Primitives.TypeTransformers as TypeCast

-- TODO replace list structure with a HashMap String ([LispVal] -> ThrowsError LispVal)
-- from Data.HashMap.Strict
-- Safe to assume we need all functions so laziness is wasted overhead
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", (Math.+))
             , ("-", (Math.-))
             , ("*", (Math.*))
             , ("/", (Math./))
             , ("mod", Math.mod)
             , ("quotient", Math.quot)
             , ("remainder", Math.rem)
             , ("symbol?", TypeCheck.isSymbol)
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
             , ("car", List.car)
             , ("cdr", List.cdr)
             , ("cons", List.cons)
             , ("char->integer", TypeCast.charToNumber)
             , ("integer->char", TypeCast.numberToChar)
             , ("number->string", TypeCast.numberToString)
             , ("string->number", TypeCast.stringToNumber)
             ]
