{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Working with timestamps.
module Fencer.Time
    ( Timestamp(..)
    , getTimestamp
    , slotBoundary
    )
where

import BasePrelude

import Data.Hashable (Hashable)
import Named ((:!), arg)
import System.Clock (getTime, Clock(Monotonic), sec)

----------------------------------------------------------------------------
-- Timestamp
----------------------------------------------------------------------------

-- | Unix timestamp with the granularity of 1 second.
newtype Timestamp = Timestamp Int64
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable, Enum)

-- | Get current time as a 'Timestamp'.
--
-- We do not want to use e.g. the 'time' library's getSystemTime
-- because it will report the same timestamp twice on a leap
-- second. The 'clock' library's getTime function with the Monotonic
-- clock cannot have negative clock jumps and report the same
-- timestamp twice.
getTimestamp :: IO Timestamp
getTimestamp = Timestamp . sec <$> getTime Monotonic

-- | Divide time into slots and round a 'Timestamp' up to a slot boundary.
--
-- >>> slotBoundary (#slotSeconds 10) (Timestamp 11)
-- Timestamp 20
--
-- If the timestamp is exactly on the boundary, it will be rounded up to the
-- next slot.
--
-- >>> slotBoundary (#slotSeconds 10) (Timestamp 20)
-- Timestamp 30
--
-- The returned timestamp is guaranteed to be greater than the passed
-- timestamp, assuming that @slotSeconds@ is positive.
slotBoundary
    :: "slotSeconds" :! Int64
    -> Timestamp
    -> Timestamp
slotBoundary (arg #slotSeconds -> slotSeconds) (Timestamp t) =
    Timestamp $ (t `div` slotSeconds + 1) * slotSeconds
