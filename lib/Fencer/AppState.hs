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
