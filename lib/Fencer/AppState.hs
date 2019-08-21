{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | In-memory state of Fencer.
module Fencer.AppState
    ( AppState(..)
    , initAppState
    )
where

import BasePrelude
import qualified StmContainers.Map as StmMap

import Fencer.Types
import Fencer.Counter
import Fencer.Match

----------------------------------------------------------------------------
-- AppState
----------------------------------------------------------------------------

-- | In-memory state.
data AppState = AppState
    { appStateRules :: !(StmMap.Map DomainId RuleTree)
    , appStateCounters :: !(StmMap.Map CounterKey Counter)
    }

-- | Create an empty 'AppState'.
initAppState :: IO AppState
initAppState = do
    appStateRules <- StmMap.newIO
    appStateCounters <- StmMap.newIO
    pure AppState{..}
