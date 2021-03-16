-------------------------------------------------------------------------------
-- Experimental R7RS Scheme parser written for Haskell using Happy.
-- (c) Max Kopinsky, 2020
--
-- A trick with laziness is used to support datum labels. Because of this, you
-- cannot make any parsing decisions / Alex monad actions depend on whether
-- a particular label is in the completeMap. Best to use 'lookupLabel' instead
-- of messing with it manually.
--
-- Accurate location information from the lexer is used for error messages but is
-- not conveyed to the output.
--
-- See also Lexer.x.
{
module Parsers.Parser where

import Val
import Parsers.Lexer
import Data.Ratio (numerator, denominator, (%))
import Data.Complex (Complex(..))
}

%name parse Datum
%name parseSeq DatumSeq
%tokentype { Lexeme }
%monad { Alex }
-- 'alexMonadScan' is also explicitly used a few places in Lexer.x. If you
-- introduce a modified scanning function, make sure to fix all the
-- appropriate occurences, not just this one!
%lexer { alexMonadScan >>= } { L _ TokEOF }
%error { happyError }

-- strictly speaking, the 'pos' pattern in all of these is unsafe. The Happy
-- Userguide doesn't actually say that it will work, or that Happy won't use
-- the same name for something else. Similarly for $$@<pattern>. However Happy
-- only uses the bracketed parts in patterns, so it works like you'd expect.
-- When 'pos' appears in a semantic action, it comes from here.
--
-- But be wary! If a rule has multiple terminals on the LHS, then there's no
-- guarantee which 'pos' you'll get if you use the pattern.
%token
  '('              { L pos TokLParen }
  ')'              { L pos TokRParen }
  '['              { L pos TokLBracket }
  ']'              { L pos TokRBracket }
  '{'              { L pos TokLBrace }
  '}'              { L pos TokRBrace }
  '#('             { L pos TokLVector }
  '#u8('           { L pos TokLByteVector }

  symbol           { L pos (TokSymbol $$) }
  number           { L pos (TokNumber $$) }
  bool             { L pos (TokBool $$) }
  char             { L pos (TokChar $$) }
  string           { L pos (TokString $$) }

  quote            { L pos $$@TokQuote }
  backquote        { L pos $$@TokBackquote }
  comma            { L pos $$@TokComma }
  commaAt          { L pos $$@TokCommaAt }

  dot              { L pos TokDot }

  labelDef         { L pos (TokLabelDef $$) }
  labelRef         { L pos (TokLabelRef $$) }

  '#;'             { L pos TokDatumComment }

%%

Datum :: { Val }
Datum : DatumComment Datum           { $2 }
      | CommentableDatum             { $1 }

-- Note that StartDC and EndDC would be capable of handling
-- #; #; a b c -READS-> c
-- correctly, if the grammar allowed that. R7RS-small's formal
-- syntax does not allow that, but SRFI-62's does. We don't.
DatumComment :: { () }
DatumComment : StartDC 
               CommentableDatum 
               EndDC                 { () }

StartDC :: { () }
StartDC : '#;'                       {% enterDatumComment }

EndDC :: { () }
EndDC : {- empty -}                  {% exitDatumComment }

CommentableDatum :: { Val }
  : SimpleDatum                      { $1 }
  | CompoundDatum                    { $1 }
  | LabelDef                         { $1 }
  | LabelRef                         { $1 }

CompoundDatum :: { Val }
CompoundDatum
  : PrefixSugar Datum                { prefix $1 $2 }
  | List                             { $1 }
  | Vector                           { $1 }

SimpleDatum :: { Val }
SimpleDatum : symbol                 { Symbol $1 }
            | number                 { mkNumber $1 }
            | bool                   { Bool $1 }
            | char                   { Char $1 }
            | string                 { IString $1 }

LabelDef :: { Val }
LabelDef : StartDef DefDatum         {% do 
                                          endDef
                                          unlessDatumComment (defineLabel $1 $2)
                                          return $2 }

LabelRef :: { Val }
LabelRef : labelRef                  {% lookupLabel $1 }

-- First part of handling #0=#0# errors.
StartDef :: { Int }
StartDef : labelDef                  {% startDef pos $1 >> return $1 }

{- This is used to handle the #0=#0# error. The idea is that we want
to parse a regular old Datum, but if the datum is exactly a label
reference we want to check that it's a well-defined reference.
An ill-defined reference is like #0=#0#. Because of our lazy method
of handling datum labels, ill-defined references make the parser hang.
There are two ways for a reference to be ill-defined:
 (1) #0 = #0#
 (2) #0 = ( . #0# ) (or nested arbitrarily far).

In the interest of producing good error messages, we make a tiny
deviation from R7RS here (which we could fix with some effort).
We won't accept #0= #1= <datum> even though it's technically valid.
If we fix this and see #0= #1= #0#, the generated message would need
to mention both 0 and 1 as being ill-defined.
Furthermore, note that SRFI-38 (which introduced datum labels) does
not allow this double-labeling, but the R7RS formal syntax does.

Note that 'LabelDef' handles ending the def and 'StartDef' handles
starting it, so we don't have to do either here.
-}
DefDatum :: { Val }
DefDatum : SimpleDatum               { $1 }
         | labelRef                  {% testRef $1 >> lookupLabel $1 }
         | PrefixSugar Datum         { prefix $1 $2 }
         | Vector                    { $1 }
         | DefList                   { $1 }

DefList :: { Val }
DefList : '(' DefListAux ')'         { $2 }
        | '[' DefListAux ']'         { $2 }
          -- see the comment in 'List'
        | '{' DefListAux '}'         { $2 }

DefListAux :: { Val }
DefListAux : DatumSeq                { makeImmutableList $ reverse $1 }
             -- here is where we handle the (. #0#) case; note
             -- the use of DefDatum instead of Datum.
           | dot DefDatum            { $2 }
           | DatumSeq1 dot Datum     { makeImproperImmutableList (reverse $1) $3 }

List :: { Val }
List : '(' ListAux ')'               { $2 }
     | '[' ListAux ']'               { $2 }
     -- TODO: this one depends on if infix expressions are enabled
     -- probably the right way to do that is to make the action
     -- monadic and have the action apply the rules from SRFI-105
     -- if enabled. (probably better would be to error if they
     -- are disabled...?)
     | '{' ListAux '}'               { $2 }

ListAux :: { Val }
ListAux : DatumSeq                   { makeImmutableList $ reverse $1 }
        | DatumSeq dot Datum         { makeImproperImmutableList (reverse $1) $3 }

Vector :: { Val }
Vector : '#(' DatumSeq ')'           { makeVector (reverse $2) }
       | '#u8(' ByteSeq ')'          { makeByteVector (reverse $2) }

DatumSeq :: { [Val] }
DatumSeq : {- empty -}                    { [] }
         | DatumSeq1                      { $1 }

DatumSeq1 :: { [Val] }
DatumSeq1 : DatumSeq CommentableDatum     { $2 : $1 }
          | DatumSeq1 DatumComment        { $1 }

ByteSeq :: { [Byte] }
ByteSeq : {- empty -}                     { [] }
        | ByteSeq number                  {% do 
                                               byte <- validateByte pos $2 
                                               return (byte : $1) }

PrefixSugar :: { Token }
PrefixSugar : quote                  { $1 }
            | backquote              { $1 }
            | comma                  { $1 }
            | commaAt                { $1 }

{
happyError (L p t) =
  alexErrorWithPos p $ "parse error at token '" ++ show t ++ "'"

parseDatum :: String -> String -> Either String Val
parseDatum = runAlex' parse

parseDatumSeq :: String -> String -> Either String [Val]
parseDatumSeq = runAlex' parseSeq

prefix :: Token -> Val -> Val
prefix tok v = makeImmutableList [Symbol (desugar tok), v]
  where
    desugar TokQuote     = "quote"
    desugar TokBackquote = "quasiquote"
    desugar TokComma     = "unquote"
    desugar TokCommaAt   = "unquote-splicing"

mkNumber :: Numeric -> Val
mkNumber (LitInteger i)  = Number $ Real $ Bignum i
mkNumber (LitRational r) = Number $ Real $ Ratnum r
mkNumber (LitFloating f) = Number $ Real $ Flonum f
mkNumber (LitComplex l r) = Number $ Complex $ 
  (numericToNumber l) :+ (numericToNumber r)

numericToNumber :: Numeric -> RealNumber
numericToNumber (LitInteger i)  = Bignum i
numericToNumber (LitRational r) = Ratnum r
numericToNumber (LitFloating f) = Flonum f

validateByte :: AlexPosn -> Numeric -> Alex Byte
validateByte p n = case n of
  LitInteger i -> checkSize i
  LitRational r
    | denominator r == 1 -> checkSize $ numerator r
    | otherwise -> fail
  LitComplex real imag -> do checkExactZero imag
                             validateByte p real
  _other -> fail
  where
    checkSize :: Integer -> Alex Byte
    checkSize i
      | i >= 0 && i <= 255 = return $ fromInteger i
      | otherwise = fail

    checkExactZero :: Numeric -> Alex ()
    checkExactZero (LitInteger 0) = pure ()
    checkExactZero (LitRational r)
      | r == 0 % 1 = pure ()
      | otherwise = fail
    checkExactZero _ = fail

    fail :: Alex a
    fail = alexErrorWithPos p $ show n ++ " is not an exact byte value"

}