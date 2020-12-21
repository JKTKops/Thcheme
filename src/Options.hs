{-|
This is a very nascent module for language options. Initially Opts only existed
as a way to enable/disable the full stack history; however this is a better
way to organize language options and accessing them. This module was factored
out of Types.hs.
-}
module Options where

import qualified Data.Set as S

data Opt
  = FullStackTrace
  | Lint -- ^ lint certain functions and/or the state of the runtime.
         -- enabling this option causes Thcheme to crash if it detects
         -- certain invariant violations.
  deriving (Eq, Ord, Read)

type Opts = S.Set Opt

noOpts :: Opts
noOpts = S.empty

setOpt :: Opt -> Opts -> Opts
setOpt = S.insert

unsetOpt :: Opt -> Opts -> Opts
unsetOpt = S.delete

checkOpt :: Opt -> Opts -> Bool
checkOpt = S.member

class HasOpts m where
  getOpts :: m Opts
  setOpts :: Opts -> m ()

modifyOpts :: (Monad m, HasOpts m) => (Opts -> Opts) -> m ()
modifyOpts f = getOpts >>= setOpts . f

enableOpt :: (Monad m, HasOpts m) => Opt -> m ()
enableOpt = modifyOpts . setOpt

disableOpt :: (Monad m, HasOpts m) => Opt -> m ()
disableOpt = modifyOpts . unsetOpt

ifOpt :: (Monad m, HasOpts m)
      => Opt -- ^ option to test
      -> m a -- ^ do if option is set
      -> m a -- ^ do if option is not set
      -> m a
ifOpt opt t f = do
  opts <- getOpts
  if checkOpt opt opts then t else f

whenOpt :: (Monad m, HasOpts m) => Opt -> m () -> m ()
whenOpt opt m = ifOpt opt m (pure ())

unlessOpt :: (Monad m, HasOpts m) => Opt -> m () -> m ()
unlessOpt opt = ifOpt opt (pure ())
