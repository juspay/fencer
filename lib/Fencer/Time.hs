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
import Named
import Data.Time.Clock.System

----------------------------------------------------------------------------
-- Timestamp
----------------------------------------------------------------------------

-- | Unix timestamp with the granularity of 1 second.
newtype Timestamp = Timestamp Int64
    deriving stock (Eq, Ord, Show)

-- | Get current time as a 'Timestamp'.
getTimestamp :: IO Timestamp
getTimestamp = Timestamp . systemSeconds <$> getSystemTime

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
slotBoundary
    :: "slotSeconds" :! Int64
    -> Timestamp
    -> Timestamp
slotBoundary (arg #slotSeconds -> slotSeconds) (Timestamp t) =
    Timestamp $ (t `div` slotSeconds + 1) * slotSeconds
