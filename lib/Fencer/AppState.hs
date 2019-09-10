{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | In-memory state of Fencer.
module Fencer.AppState
    ( AppState(..)
    , initAppState
    )
where

import BasePrelude

import qualified StmContainers.Map as StmMap

import Fencer.Types
import Fencer.Settings
import Fencer.Counter

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
data AppState = AppState
    { appStateSettings :: !Settings
    , appStateRules :: !(StmMap.Map DomainId RuleTree)
    , appStateCounters :: !(StmMap.Map CounterKey Counter)
    }

-- | Initialize the environment.
--
-- * Read 'appStateSettings' from environment variables.
-- * Set 'appStateRules' and 'appStateCounters' to empty values.
initAppState :: IO AppState
initAppState = do
    appStateRules <- StmMap.newIO
    appStateCounters <- StmMap.newIO
    appStateSettings <- getSettingsFromEnvironment
    pure AppState{..}
