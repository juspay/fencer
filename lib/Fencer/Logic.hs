{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

-- | In-memory state of Fencer and the logic for managing it.
module Fencer.Logic
    ( AppState
    , initAppState

    -- * Methods for working with 'AppState'
    , getLimit
    , getCounter
    , setRules
    , getAppStateRulesLoaded
    , updateCurrentTime
    , deleteCountersWithExpiry
    , updateLimitCounter
    )
where

import BasePrelude

import qualified StmContainers.Map as StmMap
import qualified StmContainers.Multimap as StmMultimap
import qualified StmContainers.Set as StmSet
import qualified ListT as ListT
import Named ((:!), arg)
import qualified Focus as Focus
import Control.Monad.Trans.Class (lift)
import System.Metrics (newStore, Store)

import Fencer.Counter
import Fencer.Rules
import Fencer.Time
import Fencer.Types

-- | Fencer runtime context and in-memory state.
--
-- A note on @stm-containers@ here: we use STM for 'appStateCounters'
-- because updating a counter ('Fencer.Counter.updateCounter') has to be an
-- atomic operation. If we have two parallel requests that both create a new
-- counter, we might end up with two counters. So at the very least we want
-- a @HashMap@ in a @TVar@, but "StmContainers.Map@ provides a better map -
-- one where parallel queries can create entries without write-locking the
-- entire map. See <http://nikita-volkov.github.io/stm-containers/> for more
-- details.
--
-- __Invariants:__
--
-- INV001: 'appStateCounters' and 'appStateCounterExpiry' contain the same
-- set of counter keys.
--
-- INV002: 'appStateCounters' and 'appStateCounterExpiry' are in agreement
-- about counter expiry dates.
--
-- INV003: when a counter is created, its expiry date will be strictly
-- greater than 'appStateCurrentTime'.
--
-- Note that there is no invariant that 'appStateCounters' will not have
-- expired counters. We start removing counters after incrementing
-- 'appStateCurrentTime', but we do not do it atomically - see 'tick'.
data AppState = AppState
    { -- | All active ratelimiting rules.
      appStateRules :: !(StmMap.Map DomainId RuleTree)
      -- | Whether the rules were ever loaded successfully. If not, Fencer
      -- will have to return an error on all requests.
    , appStateRulesLoaded :: !(TVar Bool)
      -- | Current time, updated every second by a dedicated thread.
    , appStateCurrentTime :: !(TVar Timestamp)
      -- | All alive counters, as well as some counters that got expired but
      -- were not removed yet.
    , appStateCounters :: !(StmMap.Map CounterKey Counter)
      -- | All counters, indexed by expiry date.
    , appStateCounterExpiry :: !(StmMultimap.Multimap Timestamp CounterKey)
      -- | A metrics store for the statsd server.
    , appStateMetricsStore :: !Store
    }

-- | Initialize the environment.
--
-- * Set all maps to empty values.
-- * Set 'appStateRulesLoaded' to @False@.
-- * Set 'appStateCurrentTime' to the current time.
initAppState :: IO AppState
initAppState = do
    appStateRules <- StmMap.newIO
    appStateRulesLoaded <- newTVarIO False
    appStateCurrentTime <- newTVarIO =<< getTimestamp
    appStateCounters <- StmMap.newIO
    appStateCounterExpiry <- StmMultimap.newIO
    appStateMetricsStore <- newStore
    pure AppState{..}

-- | Apply hits to a counter.
--
-- 'recordHits' will create a new counter if the counter does not exist, or
-- update an existing counter otherwise. The counter will be reset if it has
-- expired, and 'appStateCounterExpiry' will be updated.
--
-- __Implementation notes:__
--
-- INV001 is satisfied: we take care to create a new entry in
-- 'appStateCounterExpiry' when a new counter has been created. We do not
-- remove counters.
--
-- INV002 is satisfied: when 'counterExpiry' changes, we remove the old
-- counter from 'appStateCounterExpiry' and add the new counter.
--
-- INV003 is satisfied: if a new counter is created, the expiry date will be
-- greater than 'appStateCurrentTime', as guaranteed by 'initCounter'.
recordHits
    :: AppState
    -> "hits" :! Word
    -> "limit" :! RateLimit  -- ^ Current rate limit, fetched from 'appStateRules'
    -> CounterKey  -- ^ Counter to increment
    -> STM CounterStatus
recordHits appState (arg #hits -> hits) (arg #limit -> limit) counterKey = do
    -- Update 'appStateCounters'
    (mbOldCounter, newCounter, status) <-
        StmMap.focus updateCounterMap counterKey (appStateCounters appState)
    -- Update 'appStateCounterExpiry'
    case mbOldCounter of
        Nothing ->
            StmMultimap.insert counterKey (counterExpiry newCounter)
                (appStateCounterExpiry appState)
        Just oldCounter ->
            when (counterExpiry oldCounter /= counterExpiry newCounter) $ do
                StmMultimap.delete counterKey (counterExpiry oldCounter)
                    (appStateCounterExpiry appState)
                StmMultimap.insert counterKey (counterExpiry newCounter)
                    (appStateCounterExpiry appState)
    pure status
  where
    -- Update the counter corresponding to 'key', or create a new counter if
    -- it does not exist. Returns the old counter, the new counter, and
    -- counter status.
    updateCounterMap
        :: Focus.Focus Counter STM (Maybe Counter, Counter, CounterStatus)
    updateCounterMap = do
        now <- lift $ readTVar (appStateCurrentTime appState)
        mbOldCounter <- Focus.lookup
        let (newCounter, status) =
                updateCounter (#now now) (#hits hits) (#limit limit) $
                    case mbOldCounter of
                        Nothing -> initCounter (#now now) (#limit limit)
                        Just oldCounter -> oldCounter
        Focus.insert newCounter
        pure (mbOldCounter, newCounter, status)

-- | Fetch the current limit for a descriptor.
getLimit
    :: AppState
    -> DomainId
    -> [(RuleKey, RuleValue)]
    -> STM (Maybe RateLimit)
getLimit appState domain descriptor =
    StmMap.lookup domain (appStateRules appState) >>= \case
        Nothing -> pure Nothing
        Just ruleTree -> pure (applyRules descriptor ruleTree)

-- | Fetch the current counter for a descriptor.
getCounter
    :: AppState
    -> CounterKey
    -> STM (Maybe Counter)
getCounter appState counterKey =
    StmMap.lookup counterKey (appStateCounters appState)

-- | Handle a single descriptor in a 'shouldRateLimit' request.
--
-- Returns the current limit and response.
--
-- 'updateLimitCounter' will create a new counter if the counter does
-- not exist, or update an existing counter otherwise. The counter will be
-- reset if it has expired, and 'appStateCounterExpiry' will be updated.
updateLimitCounter
    :: AppState
    -> "hits" :! Word
    -> DomainId
    -> [(RuleKey, RuleValue)]
    -> STM (Maybe (RateLimit, CounterStatus))
updateLimitCounter appState (arg #hits -> hits) domain descriptor =
    getLimit appState domain descriptor >>= \case
        Nothing -> pure Nothing
        Just limit -> do
            let counterKey :: CounterKey
                counterKey = CounterKey
                  { counterKeyDomain = domain
                  , counterKeyDescriptor = descriptor
                  , counterKeyUnit = rateLimitUnit limit }
            status <- recordHits appState (#hits hits) (#limit limit) counterKey
            pure (Just (limit, status))

-- | Set 'appStateRules' and 'appStateRulesLoaded'.
--
-- The 'appStateCounters' field stays unchanged. This is in accordance
-- with the behavior of @lyft/ratelimit@.
--
-- There might be a change in rules with the same descriptors that
-- updates the value of 'requests_per_unit' (with the time unit left
-- intact), which allows a different number of requests to be
-- made. This is as expected. However, if there is a change in the
-- rate limit time unit, a new counter will be created, regardless of
-- how many requests the previous counter had used up.
setRules :: AppState -> [(DomainId, RuleTree)] -> STM ()
setRules appState rules = do
    writeTVar (appStateRulesLoaded appState) True
    StmMap.reset (appStateRules appState)
    forM_ rules $ \(domain, tree) ->
        StmMap.insert tree domain (appStateRules appState)

-- | Get 'appStateRulesLoaded'.
getAppStateRulesLoaded :: AppState -> STM Bool
getAppStateRulesLoaded appState = readTVar (appStateRulesLoaded appState)

-- | Update the value of 'appStateCurrentTime' to the current time.
--
-- Returns the previous and the current timestamp.
--
-- __Invariants:__
--
-- INV004: nobody else modifies 'appStateCurrentTime'.
--
-- INV005: 'updateCurrentTime' should not be called in parallel.
updateCurrentTime :: AppState -> IO (Timestamp, Timestamp)
updateCurrentTime appState = do
    -- Note: readTVarIO is faster than swapTVar, and most of the time we
    -- won't have to do a write, which is why we don't use swapTVar here. We
    -- assume that nobody else modifies 'appStateCurrentTime', so it's
    -- alright to write without doing a compare-and-swap.
    now <- getTimestamp
    before <- readTVarIO (appStateCurrentTime appState)
    when (now > before) $
        atomically $ writeTVar (appStateCurrentTime appState) now
    pure (before, now)

-- | Delete all counters with a given expiry date.
deleteCountersWithExpiry :: AppState -> Timestamp -> IO ()
deleteCountersWithExpiry appState timestamp = do
    mbExpired <- atomically $
        StmMultimap.lookupByKey timestamp (appStateCounterExpiry appState) <*
        StmMultimap.deleteByKey timestamp (appStateCounterExpiry appState)
    case mbExpired of
        Nothing -> pure ()
        Just expired -> do
            keys <- atomically $ ListT.toList (StmSet.listT expired)
            -- Note: we do *not* want to delete all counters in a single
            -- transaction because it might lead to nearly infinite retries.
            forM_ keys $ \key ->
                atomically $ StmMap.delete key (appStateCounters appState)
