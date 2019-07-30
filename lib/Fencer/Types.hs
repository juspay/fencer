{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fencer.Types
    (
    -- * Common types
      DomainId(..)
    , RuleKey(..)
    , RuleValue(..)
    , RateLimit(..)

    -- * Time units
    , TimeUnit(..)
    , timeUnitToSeconds

    -- * Definitions (configuration)
    , DomainDefinition(..)
    , DescriptorDefinition(..)
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Data.Text (Text)

----------------------------------------------------------------------------
-- Time units
----------------------------------------------------------------------------

data TimeUnit = Second | Minute | Hour | Day
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

-- | Return the duration of a 'TimeUnit'.
timeUnitToSeconds :: TimeUnit -> Int64
timeUnitToSeconds = \case
    Second -> 1
    Minute -> 60
    Hour -> 3600
    Day -> 86400

----------------------------------------------------------------------------
-- Rate limiting rules
----------------------------------------------------------------------------

newtype DomainId = DomainId Text
    deriving stock (Eq)
    deriving newtype (Hashable)

newtype RuleKey = RuleKey Text
    deriving stock (Eq)
    deriving newtype (Hashable)

newtype RuleValue = RuleValue Text
    deriving stock (Eq)
    deriving newtype (Hashable)

data RateLimit = RateLimit
    { rateLimitUnit :: !TimeUnit
    , rateLimitRequestsPerUnit :: !Word
    }

----------------------------------------------------------------------------
-- Definitions (configuration)
----------------------------------------------------------------------------

data DomainDefinition = DomainDefinition
    { domainDefinitionId :: !DomainId
    , domainDefinitionDescriptors :: ![DescriptorDefinition]
    }

data DescriptorDefinition = DescriptorDefinition
    { descriptorDefinitionKey :: !RuleKey
    , descriptorDefinitionValue :: !(Maybe RuleValue)
    , descriptorDefinitionRateLimit :: !(Maybe RateLimit)
    , descriptorDefinitionDescriptors :: !(Maybe [DescriptorDefinition])
    }
