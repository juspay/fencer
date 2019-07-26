{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

module Ratelimit.Main
    ( main
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Control.Concurrent.STM (atomically)
import Named
import qualified Focus as Focus
import qualified StmContainers.Map as StmMap

import Ratelimit.Types

data CounterKey = CounterKey
    { counterKeyDomain :: DomainId
    , counterKeyDescriptor :: [(RuleKey, RuleValue)]
    , counterKeyUnit :: TimeUnit
    , counterKeyBucket :: Word
    }
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

data Storage = Storage
    { counters :: StmMap.Map CounterKey Counter
    }

-- | A counter with the current value and the limit.
data Counter = Counter {-# UNPACK #-} !Word {-# UNPACK #-} !Word

-- | Verdict returned from the rate limiter.
data HitVerdict = HitOk | HitLimitReached

-- | Handle a request to the rate limiter: increment the relevant counter
-- and return the verdict.
--
-- If the counter does not exist, it will be created with the given limit.
--
-- TODO: should the counter be incremented if the limit is reached?
hit
    :: "key" :! CounterKey
    -> "limit" :! Word
    -> Storage
    -> IO HitVerdict
hit (arg #key -> key) (arg #limit -> limit) storage =
    atomically $
    StmMap.focus go key (counters storage)
  where
    go = do
        Counter curValue curLimit <- fromMaybe (Counter 0 limit) <$> Focus.lookup
        if curValue >= curLimit
            then pure HitLimitReached
            else Focus.insert (Counter (curValue + 1) curLimit) >> pure HitOk

main :: IO ()
main = do
    undefined
    -- create in-memory storage
    -- start
