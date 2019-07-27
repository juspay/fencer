{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Rate limit counters and logic for updating them.
module Ratelimit.Counter
    ( CounterKey(..)
    , Counter(..)
    , CounterStatus(..)
    , updateCounter
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Named

import Ratelimit.Types

-- | A key that identifies a counter.
data CounterKey = CounterKey
    { counterKeyDomain :: !DomainId
    , counterKeyDescriptor :: ![(RuleKey, RuleValue)]
    }
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

-- | A rate limit requests ("hits") counter.
--
-- We keep one counter per rate limit rule.
data Counter = Counter
    { -- | How many hits the counter has already recorded in the current
      -- time slot.
      counterHits :: !Word
      -- | The time slot that the counter is valid for. For example, if
      -- 'counterSlot' is 13000 and the 'counterLimit' unit is 'Day', the
      -- counter is valid for the 13000-th day since the Unix epoch.
    , counterSlot :: !Word
      -- | Counter limit, stored together with the counter for convenience.
    , counterLimit :: !RateLimit
    }

data CounterStatus = CounterStatus
    { -- | The current limit. We return it to avoid querying it again when
      -- preparing a rate limit response.
      counterCurrentLimit :: !RateLimit
      -- | How many hits can be taken before the limit is reached. Will be 0
      -- if the limit has been reached already.
    , counterRemainingLimit :: !Word
      -- | How many hits went over limit. Will be 0 if the limit has not
      -- been reached.
    , counterHitsOverLimit :: !Word
    }

-- | Handle a request to the rate limiter: increment the relevant counter
-- and return the status.
--
-- The counter is always incremented, even if the limit has been reached.
-- This matches the behavior of @lyft/ratelimit@.
updateCounter
    :: "now" :! Int64  -- ^ Seconds since the Unix epoch
    -> "hits" :! Word  -- ^ How many hits (requests) to record
    -> Counter
    -> (Counter, CounterStatus)
updateCounter (arg #now -> now) (arg #hits -> hits) counter =
    (newCounter, status)
  where
    limit :: Word
    limit = rateLimitRequestsPerUnit (counterLimit counter)

    -- The time slot that 'now' resides in. See documentation for
    -- 'counterSlot' for details.
    currentSlot :: Word
    currentSlot =
        fromIntegral $
        now `div` timeUnitToSeconds (rateLimitUnit (counterLimit counter))

    -- 'oldHits' is how many hits the counter reported before updating.
    -- 'newHits' is the new counter value.
    --
    -- If the counter is outdated (as per 'counterSlot'), we want to
    -- essentially create a new counter. In this case @oldHits == 0@ and
    -- @newHits == hits@. All hits before the current slot are ignored.
    --
    -- Note that we only want to create a new counter if the if the counter
    -- time slot is /older/ than our request's time slot. If it's newer, we
    -- can simply pretend that the request came later than it actually did.
    oldHits, newHits :: Word
    (oldHits, newHits) =
        if currentSlot > counterSlot counter
        then (0, hits)
        else (counterHits counter, counterHits counter + hits)

    newCounter :: Counter
    newCounter = counter
        { counterHits = newHits
        , counterSlot = currentSlot }

    status :: CounterStatus
    status =
        -- Note: we could use 'min' and 'max' instead of the 'if', but then
        -- we would have to use 'Int' instead of 'Word' because of
        -- underflow. Also, the code is easier to follow this way.
        if newHits <= limit
        then CounterStatus
                 { counterCurrentLimit = counterLimit counter
                 , counterRemainingLimit = limit - newHits
                 , counterHitsOverLimit = 0 }
        else CounterStatus
                 { counterCurrentLimit = counterLimit counter
                 , counterRemainingLimit = 0
                 , counterHitsOverLimit = min hits (newHits - limit) }
