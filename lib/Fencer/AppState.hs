{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | In-memory state of Fencer.
--
-- TODO: move invariant-modifying functions here. Don't export internal
-- state.
module Fencer.AppState
    ( AppState(..)
    , initAppState
    )
where

import BasePrelude

import qualified StmContainers.Map as StmMap
import qualified StmContainers.Multimap as StmMultimap

import Fencer.Types
import Fencer.Settings
import Fencer.Counter
import Fencer.Time

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
-- @INV001@: 'appStateCounters' and 'appStateCounterExpiry' contain the same
-- set of counter keys.
--
-- @INV002@: 'appStateCounters' and 'appStateCounterExpiry' are in agreement
-- about counter expiry dates.
--
-- @INV003@: when a counter is created, its expiry date will be strictly
-- greater than 'appStateCurrentTime'.
--
-- Note that there is no invariant that 'appStateCounters' will not have
-- expired counters. We start removing counters after incrementing
-- 'appStateCurrentTime', but we do not do it atomically.
data AppState = AppState
    { -- | Settings, loaded at the start of the program.
      appStateSettings :: !Settings
      -- | All active ratelimiting rules.
    , appStateRules :: !(StmMap.Map DomainId RuleTree)
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
-- * Read 'appStateSettings' from environment variables.
-- * Set all maps to empty values.
initAppState :: IO AppState
initAppState = do
    appStateSettings <- getSettingsFromEnvironment
    appStateRules <- StmMap.newIO
    appStateCurrentTime <- newTVarIO =<< getTimestamp
    appStateCounters <- StmMap.newIO
    appStateCounterExpiry <- StmMultimap.newIO
    pure AppState{..}
