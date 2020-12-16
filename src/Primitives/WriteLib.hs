-- | This module defines the 'write-xx' style primitives.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Primitives.WriteLib where

import Val

import Control.Monad.IO.Class (MonadIO(..))
import System.Mem.StableName (StableName, makeStableName)

import Control.Monad (when)
import Control.Monad.Reader
    (ReaderT(runReaderT), lift, asks)
import Control.Monad.State
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

writeSH :: Val -> IO String
writeSH v = do
  labeling <- labelDatum CyclicOnly v
  ($"") <$> runWrite labeling (writeShowS v)

writeSimpleSH :: Val -> IO String
writeSimpleSH v =
  ($"") <$> runWrite M.empty (writeShowS v)

writeSharedSH :: Val -> IO String
writeSharedSH v = do
  labeling <- labelDatum FullSharing v
  ($"") <$> runWrite labeling (writeShowS v)

-- | Show a 'LispErr' in IO. Forms inside the error are shown with
-- 'write-shared'.
showErrIO :: LispErr -> IO String
showErrIO = fmap (prefix ++) . mkMsgFor
  where
    prefix = "Error: "
    mkMsgFor (UnboundVar msg name) = pure $ msg `colonAnd` name
    mkMsgFor (EvaluateDuringInit name) = pure $
      name ++ " referred to itself during initialization"
    mkMsgFor (SetImmutable tyname) = pure $ "can't set immutable " ++ tyname
    mkMsgFor (BadSpecialForm msg form) =
      (msg `colonAnd`) <$> writeSharedSH form
    mkMsgFor (NotFunction msg form) =
      (msg `colonAnd`) <$> writeSharedSH form
    mkMsgFor (NumArgs exp act) = 
      ((expectedMsg exp ++ ", found values") ++) . concat
      <$> mapM (fmap (' ':) . writeSharedSH) act
      where expectedMsg 1 = "expected 1 arg"
            expectedMsg n = "expected " ++ show n ++ " args"
    mkMsgFor (TypeMismatch exp act) = (expectedMsg exp ++) <$> writeSharedSH act
      where expectedMsg s =
              "invalid type: expected " ++ s ++ ", found "
    mkMsgFor CircularList = pure "circular list"
    mkMsgFor EmptyBody = pure "attempt to define function with no body"
    mkMsgFor (Parser parseErr) = pure $ "parser error at " ++ show parseErr
    mkMsgFor (Default msg) = pure msg
    mkMsgFor Quit = pure "quit invoked"

    colonAnd s r = s ++ ": " ++ r

-- | stablename -> (label, used?)
type FirstPassMap = M.HashMap (StableName Val) Bool
type Labeling = M.HashMap (StableName Val) Int
type FirstPass = StateT FirstPassMap IO ()
data Sharing = FullSharing | CyclicOnly

labelDatum :: Sharing -> Val -> IO Labeling
labelDatum s v = fpmToLabeling <$>
  execStateT (labelDatumWorker s v) M.empty

labelDatumWorker :: Sharing -> Val -> FirstPass
labelDatumWorker s p | pairSH p = labelPair s p
labelDatumWorker _s _notPair = pure ()

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
      CyclicOnly -> modify $ M.alter deleteUnused name
  where
    deleteUnused Nothing = Nothing
    deleteUnused (Just False) = Nothing
    deleteUnused (Just True)  = Just True

fpmToLabeling :: FirstPassMap -> Labeling
fpmToLabeling fpm = evalState (M.traverseWithKey go used) 0
  where
    go _k _v = state $ \i -> (i, i+1)
    used = M.filter id fpm

type WriteState = S.HashSet (StableName Val)
type Write a = ReaderT Labeling (StateT WriteState IO) a

runWrite :: Labeling -> Write a -> IO a
runWrite lbls m =
  flip evalStateT S.empty $ runReaderT m lbls

writeShowS :: Val -> Write ShowS
writeShowS p | pairSH p = writePair p
writeShowS v = pure $ shows v

writePair :: Val -> Write ShowS
writePair p = do
  name <- liftIO $ makeStableName p
  mlbl <- askLabel name
  case mlbl of
    Nothing -> showParen True <$> writeList p
    Just lbl -> writeLabeledPair lbl name p

writeLabeledPair :: Int -> StableName Val -> Val -> Write ShowS
writeLabeledPair lbl name p = do
  defined <- labelDefined name
  if defined
    then return $ showChar '#'
                . shows lbl
                . showChar '#'
    else do
      defineLabel name -- mark defined
      let pre = showChar '#'
              . shows lbl
              . showChar '='
      (pre .) . showParen True <$> writeList p

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

labelDefined :: StableName Val -> Write Bool
labelDefined n = gets $ S.member n

defineLabel :: StableName Val -> Write ()
defineLabel n = modify $ S.insert n

askLabel :: StableName Val -> Write (Maybe Int)
askLabel name = asks $ M.lookup name
