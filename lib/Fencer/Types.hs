{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
    , HasDescriptors(..)

    -- * Time units
    , TimeUnit(..)
    , timeUnitToSeconds

    -- * Rate limit rule configs
    , DomainDefinition(..)
    , DescriptorDefinition(..)
    , descriptorDefinitionKey
    , descriptorDefinitionValue

    -- * Rate limit rules in tree form
    , RuleTree
    , RuleBranch(..)

    -- * Server
    , Port(..)
    )
where

import BasePrelude hiding (lookup)

import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Aeson (FromJSON(..), (.:), (.:?), (.!=), withObject, withText)
import Data.HashMap.Strict (HashMap, lookup)


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
    deriving stock (Eq, Ord, Show)
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

-- | A class describing how to access descriptor definitions within a
-- type, if there are any present at all.
--
-- This class is needed for accessing descriptor definitions in a
-- uniform way both when dealing with domain definitions and when
-- dealing with descriptor definitions.
class HasDescriptors a where
  descriptorsOf :: a -> [DescriptorDefinition]

-- | Config for a single domain.
--
-- Corresponds to one YAML file.
data DomainDefinition = DomainDefinition
    { domainDefinitionId :: !DomainId
    , domainDefinitionDescriptors :: ![DescriptorDefinition]
    }
    deriving stock (Eq, Show)

-- | Config for a single rule tree.
data DescriptorDefinition where
  -- | An inner node with no rate limit
  DescriptorDefinitionInnerNode
    :: !RuleKey
    -> !(Maybe RuleValue)
    -> ![DescriptorDefinition]
       -----------------------
    -> DescriptorDefinition

  -- | A leaf node with a rate limit
  DescriptorDefinitionLeafNode
    :: !RuleKey
    -> !(Maybe RuleValue)
    -> !RateLimit
       --------------------
    -> DescriptorDefinition

deriving instance Eq DescriptorDefinition
deriving instance Show DescriptorDefinition


descriptorDefinitionKey
  :: DescriptorDefinition
  -> RuleKey
descriptorDefinitionKey (DescriptorDefinitionInnerNode k _ _) = k
descriptorDefinitionKey (DescriptorDefinitionLeafNode  k _ _) = k

descriptorDefinitionValue
  :: DescriptorDefinition
  -> Maybe RuleValue
descriptorDefinitionValue (DescriptorDefinitionInnerNode _ v _) = v
descriptorDefinitionValue (DescriptorDefinitionLeafNode  _ v _) = v


instance HasDescriptors DomainDefinition where
  descriptorsOf = domainDefinitionDescriptors

instance HasDescriptors DescriptorDefinition where
  descriptorsOf (DescriptorDefinitionLeafNode{})      = []
  descriptorsOf (DescriptorDefinitionInnerNode _ _ l) = l

instance FromJSON DomainDefinition where
    parseJSON = withObject "DomainDefinition" $ \o -> do
        domainDefinitionId <- o .: "domain"
        when (domainDefinitionId == DomainId "") $
          fail "rate limit domain must not be empty"
        domainDefinitionDescriptors <- o .:? "descriptors" .!= []
        pure DomainDefinition{..}

instance FromJSON DescriptorDefinition where
  parseJSON = withObject "DescriptorDefinition" $ \o -> do
    key <- o .: "key"
    value <- o .:? "value"
    case lookup "rate_limit" o of
      Just _ -> do
        limit <- o .: "rate_limit"
        case lookup "descriptors" o of
          Nothing -> pure $ DescriptorDefinitionLeafNode key value limit
          Just _ -> fail
            "A descriptor with a rate limit cannot have a sub-descriptor"
      Nothing ->
        case lookup "descriptors" o of
          Nothing -> fail $
            "A descriptor definition must have either a rate limit " ++
            "or sub-descriptor(s)"
          Just _ -> do
            descriptors <- o .: "descriptors"
            pure $ DescriptorDefinitionInnerNode key value descriptors

----------------------------------------------------------------------------
-- Rate limit rules in tree form
----------------------------------------------------------------------------

-- | The type for a tree of rules. It is equivalent to a list of
-- 'DescriptorDefinition's, but uses nested hashmaps and is more convenient
-- to work with.
type RuleTree = HashMap (RuleKey, Maybe RuleValue) RuleBranch

-- | A single branch in a rule tree, containing several (or perhaps zero)
-- nested rules.
data RuleBranch where
  RuleBranch :: !RuleTree -> RuleBranch
  RuleLeaf :: !RateLimit -> RuleBranch

----------------------------------------------------------------------------
-- Fencer server
----------------------------------------------------------------------------

-- | A network port wrapper
newtype Port = Port { unPort :: Word }
    deriving newtype (Eq, Show, Enum)
