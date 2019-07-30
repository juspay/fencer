{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Rate limit counters and logic for creating and updating them.
module Fencer.Counter
    ( CounterKey(..)
    , Counter(..)
    , CounterStatus(..)
    , newCounter
    , updateCounter
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Named

import Fencer.Types
import Fencer.Time

-- | A key that identifies a counter.
data CounterKey = CounterKey
    { counterKeyDomain :: !DomainId
    , counterKeyDescriptor :: ![(RuleKey, RuleValue)]
    }
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

-- | A rate limit requests ("hits") counter.
data Counter = Counter
    { -- | How many hits the counter has already recorded.
      counterHits :: !Word
      -- | Counter expiry date, inclusive (i.e. on 'counterExpiry' the
      -- counter is already expired).
    , counterExpiry :: !Timestamp
    }

data CounterStatus = CounterStatus
    { -- | How many hits can be taken before the limit is reached. Will be 0
      -- if the limit has been reached already.
      counterRemainingLimit :: !Word
      -- | How many hits went over limit. Will be 0 if the limit has not
      -- been reached.
    , counterHitsOverLimit :: !Word
    }

-- | Create an empty counter.
newCounter
    :: "now" :! Timestamp -- ^ Current time
    -> "limit" :! RateLimit -- ^ Rate limit
    -> Counter
newCounter (arg #now -> now) (arg #limit -> limit) =
    Counter
        { counterHits = 0
        , counterExpiry = slotBoundary (#slotSeconds unitDuration) now }
  where
    unitDuration :: Int64
    unitDuration = timeUnitToSeconds (rateLimitUnit limit)

-- | Handle a request to the rate limiter: increment the relevant counter
-- and return the status.
--
-- The counter is always incremented, even if the limit has been reached.
-- This matches the behavior of @lyft/ratelimit@.
updateCounter
    :: "now" :! Timestamp -- ^ Current time
    -> "hits" :! Word -- ^ How many hits (requests) to record
    -> "limit" :! RateLimit -- ^ Rate limit
    -> Counter
    -> (Counter, CounterStatus)
updateCounter (arg #now -> now) (arg #hits -> hits) (arg #limit -> limit) counter =
    (newCounter, updateStatus)
  where
    -- Deconstructing the 'limit', for convenience.
    limitPerUnit :: Word
    limitPerUnit = rateLimitRequestsPerUnit limit

    unitDuration :: Int64
    unitDuration = timeUnitToSeconds (rateLimitUnit limit)

    -- New counter expiry time. It would be faster to simply add
    -- 'unitDuration' to 'counterExpiry' if the counter has expired, but if
    -- the rate limit unit has changed in the meantime, this would be a
    -- mistake.
    newCounterExpiry :: Timestamp
    newCounterExpiry = slotBoundary (#slotSeconds unitDuration) now

    -- Updated counter. If the counter is outdated (as per 'counterExpiry'),
    -- we reset it.
    newCounter :: Counter
    newCounter = Counter
        { counterHits =
              if now > counterExpiry counter
              then hits
              else counterHits counter + hits
        , counterExpiry = newCounterExpiry
        }

    updateStatus :: CounterStatus
    updateStatus =
        -- Note: we could use 'min' and 'max' instead of the 'if', but then
        -- we would have to use 'Int' instead of 'Word' because of
        -- underflow. Also, the code is easier to follow this way.
        let newHits = counterHits newCounter
        in if newHits <= limitPerUnit
           then CounterStatus
                    { counterRemainingLimit = limitPerUnit - newHits
                    , counterHitsOverLimit = 0 }
           else CounterStatus
                    { counterRemainingLimit = 0
                    , counterHitsOverLimit = min hits (newHits - limitPerUnit) }
