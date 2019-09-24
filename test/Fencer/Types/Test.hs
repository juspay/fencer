{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Tests for types from the 'Fencer.Types' module.
module Fencer.Types.Test
  ( test_parseJSONDescriptorDefinition
  , test_parseJSONDomainDefinition
  )
where

import           BasePrelude

import           Data.Aeson (parseJSON)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (parseEither, Value(..))
import           Fencer.Types (DescriptorDefinition(..), DomainDefinition(..), DomainId(..), RateLimit(..), RuleKey(..), RuleValue(..), TimeUnit(..))
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)


dd1 :: Value
dd1 = [aesonQQ| {key: "some key", value: "some value"} |]

dd2 :: Value
dd2 = [aesonQQ| {key: "some key #2", value: "some value #2", rate_limit: {unit: "second", requests_per_unit: 5}, descriptors: [#{dd1}]} |]

o :: Value
o = [aesonQQ| {domain: "some domain", descriptors: [#{dd1}, #{dd2}]} |]

test_parseJSONDescriptorDefinition :: TestTree
test_parseJSONDescriptorDefinition =
  testCase "Successful JSON parsing of DescriptorDefinition" $
    assertEqual
      "parsing DescriptorDefinition failed"
      (Right expected)
      (parseEither (parseJSON @DescriptorDefinition) dd1)
 where
  expected :: DescriptorDefinition
  expected = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key"
    , descriptorDefinitionValue = Just $ RuleValue "some value"
    , descriptorDefinitionRateLimit = Nothing
    , descriptorDefinitionDescriptors = Nothing
    }

test_parseJSONDomainDefinition :: TestTree
test_parseJSONDomainDefinition =
  testCase "Successful JSON parsing of DomainDefinition" $
    assertEqual "parsing DomainDefinition failed"
    (Right expected)
    (parseEither (parseJSON @DomainDefinition) o)
 where
  expected :: DomainDefinition
  expected = DomainDefinition
    { domainDefinitionId = DomainId "some domain"
    , domainDefinitionDescriptors = [dd1', dd2']
    }
  dd1' :: DescriptorDefinition
  dd1' = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key"
    , descriptorDefinitionValue = Just $ RuleValue "some value"
    , descriptorDefinitionRateLimit = Nothing
    , descriptorDefinitionDescriptors = Nothing
    }
  dd2' :: DescriptorDefinition
  dd2' = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key #2"
    , descriptorDefinitionValue = Just $ RuleValue "some value #2"
    , descriptorDefinitionRateLimit = Just $ RateLimit Second 5
    , descriptorDefinitionDescriptors = Just [dd1']
    }
