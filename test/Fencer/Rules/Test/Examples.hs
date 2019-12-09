{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Values used for rule and server testing.
module Fencer.Rules.Test.Examples
  ( domainDescriptorKeyValue
  , domainDescriptorKeyValueText
  , domainDescriptorKey
  , domainDescriptorKeyText
  , faultyDomain
  , minimalDomain
  , minimalDomainText
  , separatorDomain
  , separatorDomainText
  , duplicateRuleDomain
  )
  where

import           BasePrelude

import           Data.Text (Text)
import           NeatInterpolation (text)

import           Fencer.Types


-- | A leaf descriptor definition with a key, a value and a rate
-- limit.
descriptorKeyValue :: DescriptorDefinition
descriptorKeyValue = DescriptorDefinitionLeafNode
  { descriptorDefinitionKey = RuleKey "some key"
  , descriptorDefinitionValue = Just $ RuleValue "some value"
  , descriptorDefinitionRateLimit = RateLimit Hour 10
  }

-- | A leaf inner descriptor definition with a key and rate limit
-- only.
descriptorKey :: DescriptorDefinition
descriptorKey = DescriptorDefinitionLeafNode
  { descriptorDefinitionKey = RuleKey "some key 2"
  , descriptorDefinitionValue = Nothing
  , descriptorDefinitionRateLimit = RateLimit Minute 20
  }

-- | A domain definition with a single descriptor with a key and
-- value.
domainDescriptorKeyValue :: DomainDefinition
domainDescriptorKeyValue = DomainDefinition
  { domainDefinitionId = DomainId "domain1"
  , domainDefinitionDescriptors = [descriptorKeyValue]
  }

-- | The text value corresponding to 'domainDescriptorKeyValue'.
domainDescriptorKeyValueText :: Text
domainDescriptorKeyValueText = [text|
  domain: domain1
  descriptors:
    - key: some key
      value: some value
      rate_limit:
        unit: hour
        requests_per_unit: 10
  |]

-- | A domain definition with a single descriptor with a key.
domainDescriptorKey :: DomainDefinition
domainDescriptorKey = DomainDefinition
  { domainDefinitionId = DomainId "domain2"
  , domainDefinitionDescriptors = [descriptorKey]
  }

domainDescriptorKeyText :: Text
domainDescriptorKeyText = [text|
  domain: domain2
  descriptors:
    - key: some key 2
      rate_limit:
        unit: minute
        requests_per_unit: 20
  |]

-- | A faulty domain text. The text has "keyz" instead of "key", which
-- makes domain parsers fail.
faultyDomain :: Text
faultyDomain = [text|
  domain: another
  descriptors:
    - key: key2
      rate_limit:
        unit: minute
        requests_per_unit: 20
    - keyz: key3
      rate_limit:
        unit: hour
        requests_per_unit: 10
  |]

-- | A minimal domain definition comprised of the domain ID only.
minimalDomain :: DomainDefinition
minimalDomain = DomainDefinition
  { domainDefinitionId = DomainId "min"
  , domainDefinitionDescriptors = []
  }

-- | The text value corresponding to 'minimalDomain'.
minimalDomainText :: Text
minimalDomainText = [text| domain: min |]

-- | A domain definition with one key with a value and one key without
-- a value. The result of parsing 'separatorDomainText' has to be this
-- value.
separatorDomain :: DomainDefinition
separatorDomain = DomainDefinition
  { domainDefinitionId = DomainId "another"
  , domainDefinitionDescriptors = [descriptorKeyValue, descriptorKey]
  }

-- | The text value that starts with a YAML document separator. It
-- corresponds to 'separatorDomain'.
separatorDomainText :: Text
separatorDomainText = [text|
  ---
  domain: another
  descriptors:
    - key: some key
      value: some value
      rate_limit:
        unit: hour
        requests_per_unit: 10
    - key: some key 2
      rate_limit:
        unit: minute
        requests_per_unit: 20
  |]

-- | The text value of a faulty domain definition that has a key
-- repeated.
duplicateRuleDomain :: Text
duplicateRuleDomain = [text|
  domain: another
  descriptors:
    - key: key1
      rate_limit:
        unit: minute
        requests_per_unit: 20
    - key: key2
      rate_limit:
        unit: minute
        requests_per_unit: 30
    - key: key1
      rate_limit:
        unit: hour
        requests_per_unit: 10
  |]
