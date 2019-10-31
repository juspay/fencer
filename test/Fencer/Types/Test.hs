{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Tests for types from the "Fencer.Types" module.
module Fencer.Types.Test (tests) where

import           BasePrelude

import           Data.Aeson (parseJSON)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (parseEither, Value(..))
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Fencer.Types (DescriptorDefinition(..), DomainDefinition(..), DomainId(..), RateLimit(..), RuleKey(..), RuleValue(..), TimeUnit(..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)


tests :: TestTree
tests = testGroup "Type tests"
  [ test_parseJSONDescriptorDefinition
  , test_parseJSONDomainDefinition
  , test_parseJSONDomainAtLeastOneDescriptor
  , test_parseJSONNonEmptyDomainId
  , test_parseJSONOptionalDescriptorFields
  ]

descriptor1 :: Value
descriptor1 = [aesonQQ|
  {
    "key": "some key",
    "value": "some value"
  }
  |]

test_parseJSONDescriptorDefinition :: TestTree
test_parseJSONDescriptorDefinition =
  testCase "Successful JSON parsing of DescriptorDefinition" $
    assertEqual
      "parsing DescriptorDefinition failed"
      (Right expected)
      (parseEither (parseJSON @DescriptorDefinition) descriptor1)
 where
  expected :: DescriptorDefinition
  expected = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key"
    , descriptorDefinitionValue = Just $ RuleValue "some value"
    , descriptorDefinitionRateLimit = Nothing
    , descriptorDefinitionDescriptors = Nothing
    }

descriptor2 :: Value
descriptor2 = [aesonQQ|
  {
    "key": "some key #2",
    "value": "some value #2",
    "rate_limit": {
      "unit": "second",
      "requests_per_unit": 5
    },
    "descriptors": [#{descriptor1}]
  }
  |]

domain1 :: Value
domain1 = [aesonQQ|
  {
    "domain": "some domain",
    "descriptors": [#{descriptor1}, #{descriptor2}]
  }
  |]

test_parseJSONDomainDefinition :: TestTree
test_parseJSONDomainDefinition =
  testCase "Successful JSON parsing of DomainDefinition" $
    assertEqual "parsing DomainDefinition failed"
    (Right expected)
    (parseEither (parseJSON @DomainDefinition) domain1)
 where
  expected :: DomainDefinition
  expected = DomainDefinition
    { domainDefinitionId = DomainId "some domain"
    , domainDefinitionDescriptors = Just $ descriptor1' :| [descriptor2']
    }
  descriptor1' :: DescriptorDefinition
  descriptor1' = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key"
    , descriptorDefinitionValue = Just $ RuleValue "some value"
    , descriptorDefinitionRateLimit = Nothing
    , descriptorDefinitionDescriptors = Nothing
    }
  descriptor2' :: DescriptorDefinition
  descriptor2' = DescriptorDefinition
    { descriptorDefinitionKey = RuleKey "some key #2"
    , descriptorDefinitionValue = Just $ RuleValue "some value #2"
    , descriptorDefinitionRateLimit = Just $ RateLimit Second 5
    , descriptorDefinitionDescriptors = Just [descriptor1']
    }

test_parseJSONDomainAtLeastOneDescriptor :: TestTree
test_parseJSONDomainAtLeastOneDescriptor =
  testCase "DomainDefinition has to have at least one descriptor" $
    assertEqual "parsing DomainDefinition failed"
      (Left "Error in $.descriptors: parsing NonEmpty failed, unexpected empty list")
      (parseEither (parseJSON @DomainDefinition) domain)
 where
  domain :: Value
  domain = [aesonQQ|
    {
      "domain": "some domain #2",
      "descriptors": []
    }
    |]

test_parseJSONNonEmptyDomainId :: TestTree
test_parseJSONNonEmptyDomainId =
  testCase "DomainId cannot be empty" $
    assertEqual "parsing DomainDefinition failed"
      (Left "Error in $: rate limit domain must not be empty")
      (parseEither (parseJSON @DomainDefinition) domain)
 where
  domain :: Value
  domain = [aesonQQ|
    {
      "domain": "",
      "descriptors": [#{descriptor1}]
    }
    |]

test_parseJSONOptionalDescriptorFields :: TestTree
test_parseJSONOptionalDescriptorFields =
  testCase "Optional descriptor definition fields" $
    assertEqual "parsing DomainDefinition failed"
      (Right domain)
      (parseEither (parseJSON @DomainDefinition) domainValue)
 where
  desc1Value :: Value
  desc1Value = [aesonQQ|
    {
      "key": "key #1"
    , "value": "value #1"
    , "descriptors": [
        {"key": "inner key #1", "rate_limit": {"unit": "minute", "requests_per_unit": 10}}]
    }
    |]
  desc1 :: DescriptorDefinition
  desc1 = DescriptorDefinition
    {
      descriptorDefinitionKey         = RuleKey "key #1"
    , descriptorDefinitionValue       = Just . RuleValue $ "value #1"
    , descriptorDefinitionRateLimit   = Nothing
    , descriptorDefinitionDescriptors = Just [
        DescriptorDefinition
          {
            descriptorDefinitionKey         = RuleKey "inner key #1"
          , descriptorDefinitionValue       = Nothing
          , descriptorDefinitionRateLimit   = Just RateLimit {rateLimitUnit = Minute, rateLimitRequestsPerUnit = 10}
          , descriptorDefinitionDescriptors = Nothing
          }
        ]
    }
  desc2Value :: Value
  desc2Value = [aesonQQ|
    {
      "key": "key #2"
    , "rate_limit": {"unit": "hour", "requests_per_unit": 1000}
    }
    |]
  desc2 :: DescriptorDefinition
  desc2 = DescriptorDefinition
    {
      descriptorDefinitionKey         = RuleKey "key #2"
    , descriptorDefinitionValue       = Nothing
    , descriptorDefinitionRateLimit   = Just RateLimit {rateLimitUnit = Hour, rateLimitRequestsPerUnit = 1000}
    , descriptorDefinitionDescriptors = Nothing
    }
  domainValue :: Value
  domainValue = [aesonQQ|
    {
      "domain": "messaging"
    , "descriptors": [#{desc1Value}, #{desc2Value}]
    }
    |]
  domain :: DomainDefinition
  domain = DomainDefinition
    {
      domainDefinitionId          = DomainId "messaging"
    , domainDefinitionDescriptors = Just $ desc1 :| [desc2]
    }
