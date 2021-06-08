-- | This module defines the 'write-xx' style primitives.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Primitives.WriteLib 
  ( writeSH, writeSimpleSH, writeSharedSH, displaySH
  , showErrIO

  , ushowString
  ) where

import Val
import Primitives.Vector (vectorElemsPH)
import Primitives.String (stringSH, unwrapStringPH)

import System.Mem.StableName (StableName, makeStableName)

import Control.Monad.Reader (ReaderT(runReaderT), asks)
import Control.Monad.State
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import GHC.Show (showLitChar)
import Data.Char (isPrint)
import Data.Complex (Complex(..))
import Data.Ratio (numerator, denominator)

writeSH :: Val -> IO String
writeSH v = do
  labeling <- labelDatum CyclicOnly v
  runShowS <$> withWriteComponents labeling (writeShowS v)

writeSimpleSH :: Val -> IO String
writeSimpleSH v =
  runShowS <$> withWriteComponents M.empty (writeShowS v)

writeSharedSH :: Val -> IO String
writeSharedSH v = do
  labeling <- labelDatum FullSharing v
  runShowS <$> withWriteComponents labeling (writeShowS v)

displaySH :: Val -> IO String
displaySH v = do
  labeling <- labelDatum CyclicOnly v
  runShowS <$> withDisplayComponents labeling (writeShowS v)

-- | Run a 'Write' action with the given labeling, using
-- component writers for the 'writeSH' family.
withWriteComponents :: Labeling -> Write a -> IO a
withWriteComponents = runWrite charHH stringHH symbolHH

-- | Run a 'Write' action with the given labeling, using
-- component writers for the 'displaySH' function.
withDisplayComponents :: Labeling -> Write a -> IO a
withDisplayComponents = runWrite writeCharHH writeStringHH writeSymbolHH

runShowS :: ShowS -> String
runShowS f = f ""

-- | Show a 'LispErr' in IO. Forms inside the error are shown with
-- 'write-shared'.
showErrIO :: LispErr -> IO String
showErrIO = fmap (prefix ++) . mkMsgFor
  where
    prefix = "Error: "
    mkMsgFor (UnboundVar msg name)
      = pure $ msg ++ " unbound symbol" `colonAnd` symbolAsString name
    mkMsgFor (EvaluateDuringInit name) = pure $
      symbolAsString name ++ " referred to itself during initialization"
    mkMsgFor (SetImmutable tyname) = pure $ "can't set immutable " ++ tyname
    mkMsgFor (BadSpecialForm form) =
      ("bad form" `colonAnd`) <$> writeSharedSH form
    mkMsgFor (NotFunction msg form) =
      (msg `colonAnd`) <$> writeSharedSH form
    mkMsgFor (NumArgs arity act) = 
      ((expectedMsg arity ++ ", found values") ++) . concat
      <$> mapM (fmap (' ':) . writeSharedSH) act
      where expectedMsg a = "expected " ++ showArity a ++ " arg" ++ p
              where p | usePluralForArity a = "s"
                      | otherwise = ""
    mkMsgFor (TypeMismatch exp act) = (expectedMsg exp ++) <$> writeSharedSH act
      where expectedMsg s =
              "invalid type: expected " ++ s ++ ", found "
    mkMsgFor CircularList = pure "circular list"
    mkMsgFor EmptyBody = pure "attempt to define function with no body"
    mkMsgFor (Parser parseErr) = pure $ "parser error at " ++ show parseErr
    mkMsgFor (Condition _ obj) = writeSharedSH obj
    mkMsgFor (Default msg) = pure msg
    mkMsgFor Quit = pure "quit invoked"

    colonAnd s r = s ++ ": " ++ r

-------------------------------------------------------------------------------
-- HH versions of Scheme primitive functions
-- These are NOT the conversion functions used by writeSH!
-------------------------------------------------------------------------------

writeCharHH :: Char -> ShowS
writeCharHH c = (c:)

writeStringHH :: Text -> ShowS
writeStringHH t = showString $ unpack t

writeSymbolHH :: Symbol -> ShowS
writeSymbolHH t = showString $ unpack t

-------------------------------------------------------------------------------
-- HH conversion functions used by writeSH and friends
-------------------------------------------------------------------------------

charHH :: Char -> ShowS
charHH = shows . Char

stringHH :: Text -> ShowS
-- ushowString behaves like shows, not showString.
stringHH t = ushowString $ unpack t

-- TODO [r7rs]
-- these should be escaped according to r7rs, but we don't want
-- them to be escaped at the REPL prompt. 'repl-write'?
symbolHH :: Symbol -> ShowS
symbolHH t = showString $ unpack t

-------------------------------------------------------------------------------
-- Core implementation
-------------------------------------------------------------------------------

-- | stablename -> (label, used?)
type FirstPassMap = M.HashMap (StableName Val) Bool
type Labeling = M.HashMap (StableName Val) Int
type FirstPass = StateT FirstPassMap IO ()
data Sharing = FullSharing | CyclicOnly

labelDatum :: Sharing -> Val -> IO Labeling
labelDatum s v = fpmToLabeling <$>
  execStateT (labelDatumWorker s v) M.empty

labelDatumWorker :: Sharing -> Val -> FirstPass
labelDatumWorker s p 
  | pairSH p = labelPair s p
  | vectorSH p = labelVector s p
labelDatumWorker _s _notNested = pure ()

-- | Calling this on a non-pair will panic!
labelPair :: Sharing -> Val -> FirstPass
labelPair sharing pair = do
  name <- lift $ makeStableName pair
  keepGoing <- state $ \m -> case M.lookup name m of
        Just False -> (False, M.adjust (const True) name m)
        Just True  -> (False, m)
        Nothing -> (True, M.insert name False m)
  when keepGoing $ do
    car <- carPS pair
    labelDatumWorker sharing car
    cdr <- cdrPS pair
    labelDatumWorker sharing cdr
    case sharing of
      FullSharing -> pure ()
      CyclicOnly -> modify $ deleteUnused name

-- | See 'labelPair'.
labelVector :: Sharing -> Val -> FirstPass
labelVector sharing vec = do
  name <- lift $ makeStableName vec
  keepGoing <- state $ \m -> case M.lookup name m of
    Just False -> (False, M.adjust (const True) name m)
    Just True  -> (False, m)
    Nothing    -> (True, M.insert name False m)
  when keepGoing $ do
    vecElems <- vectorElemsPH vec
    mapM_ (labelDatumWorker sharing) vecElems
    case sharing of
      FullSharing -> pure ()
      CyclicOnly  -> modify $ deleteUnused name

deleteUnused :: StableName Val -> FirstPassMap -> FirstPassMap
deleteUnused = M.alter f
  where
    f Nothing = Nothing
    f (Just False) = Nothing
    f (Just True)  = Just True



fpmToLabeling :: FirstPassMap -> Labeling
fpmToLabeling fpm = evalState (M.traverseWithKey go used) 0
  where
    go _k _v = state $ \i -> (i, i+1)
    used = M.filter id fpm

type WriteState = S.HashSet (StableName Val)
type ComponentWriter a = a -> ShowS
data WriteEnv = WriteEnv
  { labeling :: Labeling
  , writeChar :: ComponentWriter Char
  , writeString :: ComponentWriter Text
  , writeSymbol :: ComponentWriter Symbol
  }
type Write a = ReaderT WriteEnv (StateT WriteState IO) a

runWrite :: ComponentWriter Char   -- ^ how to print characters
         -> ComponentWriter Text   -- ^ how to print (Scheme) strings
         -> ComponentWriter Symbol -- ^ how to print symbols
         -> Labeling               -- ^ mapping that witnesses sharing in v
         -> Write a                -- ^ Write action that computes v -> a
         -> IO a
runWrite wc wt ws lbls m =
  flip evalStateT S.empty $ runReaderT m $ WriteEnv lbls wc wt ws

writeShowS :: Val -> Write ShowS
writeShowS v
  | pairSH v = writePair v
  | vectorSH v = writeVector v
  | stringSH v = do
    ws <- asks writeString
    ws <$> unwrapStringPH v

-- we have to write symbols in all their glory because
-- literal (quoted) symbols need to be printed correctly.
-- If they aren't printed correctly at a prompt, a user
-- would merely be confused - but if they aren't written
-- correctly to a file, then the expander can't expand itself.
writeShowS (Symbol s) = do
  ws <- asks writeSymbol
  return $ ws s

writeShowS (Char c) = do
  wc <- asks writeChar
  return $ wc c

writeShowS (Number n) = pure $ writeNumber n
  where writeNumber (Real (Bignum i)) = shows i
        writeNumber (Real (Ratnum r))
          | denominator r == 1 = shows $ numerator r
          | otherwise = shows (numerator r)
                      . showChar '/'
                      . shows (denominator r)
        writeNumber (Real (Flonum f))
          | isInfinite f = showString $ sign : "inf.0"
          | isNaN f      = showString "+nan.0"
          | otherwise    = shows f
          where sign = if f < 0 then '-' else '+'
        writeNumber (Complex (r :+ i))
          | isExactZero $ Real i = writeNumber (Real r)
          | isExactZero $ Real r = writeNumber (Real i) . showChar 'i'

          -- minus sign will be given since i is negative
          | i < 0 || isNegativeZero i = writeNumber (Real r)
                                      . writeNumber (Real i)
                                      . showChar 'i'
          | otherwise = writeNumber (Real r)
                      . (if isInfinite i || isNaN i then id else showChar '+')
                      . writeNumber (Real i)
                      . showChar 'i'

writeShowS (MultipleValues vs) = fmt <$> writeList (makeImmutableList vs)
  where
    fmt vsStr = showString "#<values: " . vsStr . showChar '>'

writeShowS (Exception e) = showString <$> liftIO (showErrIO e)
writeShowS (Error msg irritants) = (prefix (unpack msg) .) <$> writeIrritants
  where
    prefix "" = showString ""
    prefix s  = showString s . showChar ' '
    writeIrritants = case irritants of
      [] -> pure $ showString ""
      _  -> writeList $ makeImmutableList irritants

writeShowS v = pure $ shows v

writePair :: Val -> Write ShowS
writePair p = do
  name <- liftIO $ makeStableName p
  mlbl <- askLabel name
  case mlbl of
    Nothing -> showParen True <$> writeList p
    Just lbl -> writeLabeledPair lbl name p

writeVector :: Val -> Write ShowS
writeVector v = do
  name <- liftIO $ makeStableName v
  elems <- vectorElemsPH v
  mlbl <- askLabel name
  case mlbl of
    Nothing -> (showChar '#' .) . showParen True <$> writeVectorElems elems
    Just lbl -> writeLabeledVector lbl name elems

writeLabeledPair :: Int -> StableName Val -> Val -> Write ShowS
writeLabeledPair lbl name p = do
  defined <- labelDefined name
  if defined
    then return $ showLabelUse lbl
    else do
      defineLabel name -- mark defined
      let pre = showLabelDef lbl
      (pre .) . showParen True <$> writeList p

writeLabeledVector :: Int -> StableName Val -> [Val] -> Write ShowS
writeLabeledVector lbl name p = do
  defined <- labelDefined name
  if defined
    then return $ showLabelUse lbl
    else do
      defineLabel name
      let pre = showLabelDef lbl . showChar '#'
      (pre .) . showParen True <$> writeVectorElems p

showLabelUse, showLabelDef :: Int -> ShowS
showLabelUse lbl = showChar '#' . shows lbl . showChar '#'
showLabelDef lbl = showChar '#' . shows lbl . showChar '='

-- we have to be careful here to correctly handle the case where the
-- cdr needs to be labeled. If the cdr needs to be labeled, we have
-- to use dotted notation immediately. This list ends, and we print
-- the cdr (which may still need its label defined, in which case it
-- will appear as a labeled list).
writeList :: Val -> Write ShowS
writeList pair = do
  car <- liftIO $ carPS pair
  showsCar <- writeShowS car
  (showsCar .) <$> (cdrPS pair >>= writeList1)

writeList1 :: Val -> Write ShowS
writeList1 p | pairSH p = do
  name <- liftIO $ makeStableName p
  mlbl <- askLabel name
  case mlbl of
    Nothing -> (showChar ' ' .) <$> writeList p
    Just lbl -> (showString " . " .) <$>
      writeLabeledPair lbl name p
writeList1 Nil = pure id
writeList1 dot = (showString " . " .) <$> writeShowS dot

writeVectorElems :: [Val] -> Write ShowS
writeVectorElems [] = pure id
writeVectorElems [val] = writeShowS val
writeVectorElems (v:vs) = spaceSep <$> writeShowS v <*> writeVectorElems vs
  where
    spaceSep s1 s2 = s1 . showChar ' ' . s2

labelDefined :: StableName Val -> Write Bool
labelDefined n = gets $ S.member n

defineLabel :: StableName Val -> Write ()
defineLabel n = modify $ S.insert n

askLabel :: StableName Val -> Write (Maybe Int)
askLabel name = asks $ M.lookup name . labeling

-- | So this is funny.
--
-- Haskell's 'Show' instance for 'Char' defines 'showList', this is what
-- is used to 'show' Strings. If you chase the source a bit, you'll find that
-- it calls 'showLitChar' - a function that does not propogate Unicode
-- characters! Which is pretty silly. This leads to the awkward situation where
-- we print #\λ correctly, but not (list->string '(#\λ)).
--
-- This function corrects the case of 'showLitChar'
-- that escapes Unicode code points.
ushowString :: String -> ShowS
ushowString s = showChar '\"' . showLitString s . showChar '\"'
  where 
    showLitString [] s = s
    showLitString (c:cs) s = ushowLitChar c (showLitString cs s)

    ushowLitChar c s
      | c > '\DEL' && isPrint c = showChar c s
      | otherwise = showLitChar c s
