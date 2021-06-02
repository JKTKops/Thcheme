{-# LANGUAGE NumericUnderscores #-}
-- | Primitive support for the (scheme time) library.
module Primitives.Time where

import System.Clock

import Val
import EvaluationMonad

-- | Scheme primitive exports.
primitives :: [Primitive]
primitives = [currentSecondP, currentJiffyP, jiffiesPerSecondP]

currentSecondP :: Primitive
currentSecondP = Prim "current-second" (Exactly 0) $ \[] -> do
  time <- liftIO $ getTime Realtime
  return $ makeBignum $ fromIntegral $ sec time

-- | See the r7rs spec for current-jiffy. At the moment, this
-- gives the time in nanoseconds since the system started,
-- according to the system clock. However, this is not guaranteed.
currentJiffyP :: Primitive
currentJiffyP = Prim "current-jiffy" (Exactly 0) $ \[] -> do
  TimeSpec s n <- liftIO $ getTime Monotonic
  let nanos = fromIntegral s * 1_000_000_000 + fromIntegral n
  return $ makeBignum nanos

-- | See the r7rs spec for jiffies-per-second. Returns the number
-- of jiffies per real-time second. At the moment, this is 1e9,
-- as jiffies are nanoseconds, but this is not guaranteed.
jiffiesPerSecondP :: Primitive
jiffiesPerSecondP = Prim "jiffies-per-second" (Exactly 0) $
  \[] -> pure $ makeBignum 1_000_000_000
