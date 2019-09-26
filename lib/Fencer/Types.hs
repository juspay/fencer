{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types used in Fencer. We try to keep most types in one module to avoid
-- circular dependencies between modules.
module Fencer.Types
    (
    -- * Common types
    -- $sample-config
      DomainId(..)
    , unDomainId
    , RuleKey(..)
    , unRuleKey
    , RuleValue(..)
    , unRuleValue
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
import qualified Data.List.NonEmpty as NE


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

-- $sample-config
--
-- This sample config shows which config fields correspond to which types:
--
-- @
-- domain: mongo\_cps  # 'DomainId'
-- descriptors:
--   - key: database  # 'RuleKey'
--     value: users  # 'RuleValue'
--     rate\_limit: # 'RateLimit'
--       unit: second
--       requests\_per\_unit: 500
-- @

-- | Domain name. Several rate limiting rules can belong to the same domain.
newtype DomainId = DomainId Text
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

-- | Unwrap 'DomainId'.
unDomainId :: DomainId -> Text
unDomainId (DomainId s) = s

-- | A label for a branch in the rate limit rule tree.
newtype RuleKey = RuleKey Text
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

-- | Unwrap 'RuleKey'.
unRuleKey :: RuleKey -> Text
unRuleKey (RuleKey s) = s

-- | An optional value associated with a rate limiting rule.
newtype RuleValue = RuleValue Text
    deriving stock (Eq, Show)
    deriving newtype (Hashable, FromJSON)

-- | Unwrap 'RuleValue'.
unRuleValue :: RuleValue -> Text
unRuleValue (RuleValue s) = s

-- | A specification of the rate limit that should be applied to a branch in
-- the rate limit rule tree.
data RateLimit = RateLimit
    { -- | Rate limit granularity.
      rateLimitUnit :: !TimeUnit
      -- | How many requests are allowed during each 'rateLimitUnit'.
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

-- | Config for a single domain.
--
-- Corresponds to one YAML file.
data DomainDefinition = DomainDefinition
    { domainDefinitionId :: !DomainId
    , domainDefinitionDescriptors :: !(NE.NonEmpty DescriptorDefinition)
    }
    deriving stock (Eq, Show)

-- | Config for a single rule tree.
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
        when (domainDefinitionId == DomainId "") $
          fail "rate limit domain must not be empty"
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
