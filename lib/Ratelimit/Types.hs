{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratelimit.Types
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

-- TODO: are descriptor lists matched in order or as maps?

-- TODO: if the request descriptor list is longer than the definition
-- descriptor list, it will not match. What if it's shorter, though?

-- TODO: let's say I have two rules "a=* b=y" and a=x b=*". I got a request
-- "a=x b=y". Which rule will it match?

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
