{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Tests for types from the 'Fencer.Types' module.
module Fencer.Types.Test
  ( test_parseJSONDescriptorDefinition
  , test_parseJSONDomainDefinition
  , test_parseJSONDomainAtLeastOneDescriptor
  )
where

import           BasePrelude

import           Data.Aeson (parseJSON)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (parseEither, Value(..))
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Fencer.Types (DescriptorDefinition(..), DomainDefinition(..), DomainId(..), RateLimit(..), RuleKey(..), RuleValue(..), TimeUnit(..))
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)


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
    , domainDefinitionDescriptors = descriptor1' :| [descriptor2']
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

domain2 :: Value
domain2 = [aesonQQ|
  {
    "domain": "some domain #2",
    "descriptors": []
  }
  |]

test_parseJSONDomainAtLeastOneDescriptor :: TestTree
test_parseJSONDomainAtLeastOneDescriptor =
  testCase "DomainDefinition has to have at least one descriptor" $
    assertEqual "parsing DomainDefinition failed"
      (Left "Error in $.descriptors: Expected a NonEmpty but got an empty list")
      (parseEither (parseJSON @DomainDefinition) domain2)
