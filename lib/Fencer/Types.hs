{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types used in Fencer. We try to keep most types in one module to avoid
-- circular dependencies between modules.
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

    -- * Rate limit rule configs
    , DomainDefinition(..)
    , DescriptorDefinition(..)

    -- * Rate limit rules in tree form
    , RuleTree
    , RuleBranch(..)
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, withText)
import Data.HashMap.Strict (HashMap)

----------------------------------------------------------------------------
-- Time units
----------------------------------------------------------------------------

-- | All time units a rate limit could apply to.
data TimeUnit = Second | Minute | Hour | Day
    deriving stock (Eq, Generic, Show)
    deriving anyclass (Hashable)

instance FromJSON TimeUnit where
    parseJSON = withText "TimeUnit" $ \case
        "second" -> pure Second
        "minute" -> pure Minute
        "hour" -> pure Hour
        "day" -> pure Day
        other -> fail ("unknown time unit: " ++ show other)

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
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

newtype RuleKey = RuleKey Text
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

newtype RuleValue = RuleValue Text
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

data RateLimit = RateLimit
    { rateLimitUnit :: !TimeUnit
    , rateLimitRequestsPerUnit :: !Word
    }
    deriving stock (Eq, Show)

instance FromJSON RateLimit where
    parseJSON = withObject "RateLimit" $ \o -> do
        rateLimitUnit <- o .: "unit"
        rateLimitRequestsPerUnit <- o .: "requests_per_unit"
        pure RateLimit{..}

----------------------------------------------------------------------------
-- Rate limit rule configs
----------------------------------------------------------------------------

data DomainDefinition = DomainDefinition
    { domainDefinitionId :: !DomainId
    , domainDefinitionDescriptors :: ![DescriptorDefinition]
    }
    deriving stock (Eq, Show)

data DescriptorDefinition = DescriptorDefinition
    { descriptorDefinitionKey :: !RuleKey
    , descriptorDefinitionValue :: !(Maybe RuleValue)
    , descriptorDefinitionRateLimit :: !(Maybe RateLimit)
    , descriptorDefinitionDescriptors :: !(Maybe [DescriptorDefinition])
    }
    deriving stock (Eq, Show)

instance FromJSON DomainDefinition where
    parseJSON = withObject "DomainDefinition" $ \o -> do
        domainDefinitionId <- o .: "domain"
        domainDefinitionDescriptors <- o .: "descriptors"
        pure DomainDefinition{..}

instance FromJSON DescriptorDefinition where
    parseJSON = withObject "DescriptorDefinition" $ \o -> do
        descriptorDefinitionKey <- o .: "key"
        descriptorDefinitionValue <- o .:? "value"
        descriptorDefinitionRateLimit <- o .:? "rate_limit"
        descriptorDefinitionDescriptors <- o .:? "descriptors"
        pure DescriptorDefinition{..}

----------------------------------------------------------------------------
-- Rate limit rules in tree form
----------------------------------------------------------------------------

-- | The type for a tree of rules. It is equivalent to a list of
-- 'DescriptorDefinition's, but uses nested hashmaps and is more convenient
-- to work with.
type RuleTree = HashMap (RuleKey, Maybe RuleValue) RuleBranch

-- | A single branch in a rule tree, containing several (or perhaps zero)
-- nested rules.
data RuleBranch = RuleBranch
    { ruleBranchRateLimit :: !(Maybe RateLimit)
    , ruleBranchNested :: !RuleTree
    }
