{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | In-memory state of Fencer.
module Fencer.AppState
    ( AppState
    , initAppState

    -- * Methods for working with 'AppState'
    , recordHits
    , getLimit
    , setRules
    )
where

import BasePrelude

import qualified StmContainers.Map as StmMap
import qualified StmContainers.Multimap as StmMultimap
import Named ((:!), arg)
import qualified Focus as Focus
import Control.Monad.Trans.Class (lift)

import Fencer.Types
import Fencer.Counter
import Fencer.Time
import Fencer.Rules

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
-- 'appStateCurrentTime', but we do not do it atomically.
data AppState = AppState
    { -- | All active ratelimiting rules.
      appStateRules :: !(StmMap.Map DomainId RuleTree)
      -- | Current time, updated every second by a dedicated thread.
    , appStateCurrentTime :: !(TVar Timestamp)
      -- | All alive counters, as well as some counters that got expired but
      -- were not removed yet.
    , appStateCounters :: !(StmMap.Map CounterKey Counter)
      -- | All counters, indexed by expiry date.
    , appStateCounterExpiry :: !(StmMultimap.Multimap Timestamp CounterKey)
    }

-- | Initialize the environment.
--
-- * Set all maps to empty values.
-- * Set 'appStateCurrentTime' to the current time.
initAppState :: IO AppState
initAppState = do
    appStateRules <- StmMap.newIO
    appStateCurrentTime <- newTVarIO =<< getTimestamp
    appStateCounters <- StmMap.newIO
    appStateCounterExpiry <- StmMultimap.newIO
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
getLimit appState domain descriptor = do
    StmMap.lookup domain (appStateRules appState) >>= \case
        Nothing -> pure Nothing
        Just ruleTree -> pure (applyRules descriptor ruleTree)

-- | Set 'appStateRules'.
setRules :: AppState -> [(DomainId, RuleTree)] -> STM ()
setRules appState rules = do
    StmMap.reset (appStateRules appState)
    forM_ rules $ \(domain, tree) ->
        StmMap.insert tree domain (appStateRules appState)
