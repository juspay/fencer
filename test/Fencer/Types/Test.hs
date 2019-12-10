{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Tests for types from the "Fencer.Types" module.
module Fencer.Types.Test (tests) where

import           BasePrelude

import           Data.Aeson (parseJSON)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Aeson.Types (parseEither, Value(..))
import           Fencer.Types (DescriptorDefinition(..), DomainDefinition(..), DomainId(..), RateLimit(..), RuleKey(..), RuleValue(..), TimeUnit(..))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)


tests :: TestTree
tests = testGroup "Type tests"
  [ test_parseJSONDescriptorDefinition
  , test_parseJSONFaultyDescriptorDefinition1
  , test_parseJSONFaultyDescriptorDefinition2
  , test_parseJSONDomainDefinition
  , test_parseJSONDomainEmptyDescriptors
  , test_parseJSONNonEmptyDomainId
  , test_parseJSONOptionalDescriptorFields
  ]

descriptor1 :: Value
descriptor1 = [aesonQQ|
  {
    "key": "some key",
    "value": "some value",
    "rate_limit": {
      "unit": "second",
      "requests_per_unit": 5
    }
  }
  |]

-- | Test that parsing a valid leaf descriptor definition passes.
test_parseJSONDescriptorDefinition :: TestTree
test_parseJSONDescriptorDefinition =
  testCase "Successful JSON parsing of DescriptorDefinitionLeafNode" $
    assertEqual
      "parsing DescriptorDefinition failed"
      (Right expected)
      (parseEither (parseJSON @DescriptorDefinition) descriptor1)
 where
  expected :: DescriptorDefinition
  expected =
    DescriptorDefinitionLeafNode
      (RuleKey "some key")
      (Just $ RuleValue "some value")
      (RateLimit Second 5)

-- | Test that parsing an invalid inner descriptor definition fails.
test_parseJSONFaultyDescriptorDefinition1 :: TestTree
test_parseJSONFaultyDescriptorDefinition1 =
  testCase "Unsuccessful JSON parsing of an inner DescriptorDefinition" $
    assertEqual
      "Expected a specific failure, but got something else"
      (Left expectedErrMsg)
      (parseEither (parseJSON @DescriptorDefinition) faultyInnerDescriptor)
 where
  expectedErrMsg :: String
  expectedErrMsg =
    "Error in $: A descriptor with a rate limit cannot have a sub-descriptor"

-- | Test that parsing an invalid leaf descriptor definition fails.
test_parseJSONFaultyDescriptorDefinition2 :: TestTree
test_parseJSONFaultyDescriptorDefinition2 =
  testCase "Unsuccessful JSON parsing of a leaf DescriptorDefinition" $
    assertEqual
      "Expected a specific failure, but got something else"
      (Left expectedErrMsg)
      (parseEither (parseJSON @DescriptorDefinition) faultyLeafDescriptor)
 where
  expectedErrMsg :: String
  expectedErrMsg =
    "Error in $: A descriptor definition must have either a rate limit " ++
    "or sub-descriptor(s)"

descriptor2 :: Value
descriptor2 = [aesonQQ|
  {
    "key": "some key #2",
    "value": "some value #2",
    "descriptors": [#{descriptor1}]
  }
  |]

-- | An inner descriptor definition cannot have a rate limit so
-- parsing it should fail.
faultyInnerDescriptor :: Value
faultyInnerDescriptor = [aesonQQ|
  {
    "key": "some key #2",
    "value": "some value #2",
    "rate_limit": {
      "unit": "minute",
      "requests_per_unit": 11
    },
    "descriptors": [#{descriptor1}]
  }
  |]

-- | A leaf descriptor definition must have a rate limit so parsing it
-- should fail.
faultyLeafDescriptor :: Value
faultyLeafDescriptor = [aesonQQ|
  {
    "key": "some key #2",
    "value": "some value #2"
  }
  |]

domain1 :: Value
domain1 = [aesonQQ|
  {
    "domain": "some domain",
    "descriptors": [#{descriptor2}]
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
    , domainDefinitionDescriptors = [descriptor2']
    }
  descriptor1' :: DescriptorDefinition
  descriptor1' = DescriptorDefinitionLeafNode
    (RuleKey "some key")
    (Just $ RuleValue "some value")
    (RateLimit Second 5)
  descriptor2' :: DescriptorDefinition
  descriptor2' = DescriptorDefinitionInnerNode
    (RuleKey "some key #2")
    (Just $ RuleValue "some value #2")
    [descriptor1']

test_parseJSONDomainEmptyDescriptors :: TestTree
test_parseJSONDomainEmptyDescriptors =
  testCase "DomainDefinition can have an empty descriptor array" $
    assertEqual "parsing DomainDefinition failed"
      (Right $ DomainDefinition (DomainId "some domain #2") [])
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
  desc1 = DescriptorDefinitionInnerNode
    (RuleKey "key #1")
    (Just . RuleValue $ "value #1")
    [ DescriptorDefinitionLeafNode
        (RuleKey "inner key #1")
        Nothing
        (RateLimit Minute 10)
    ]
  desc2Value :: Value
  desc2Value = [aesonQQ|
    {
      "key": "key #2"
    , "rate_limit": {"unit": "hour", "requests_per_unit": 1000}
    }
    |]
  desc2 :: DescriptorDefinition
  desc2 = DescriptorDefinitionLeafNode
    (RuleKey "key #2")
    Nothing
    (RateLimit Hour 1000)
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
    , domainDefinitionDescriptors = [desc1, desc2]
    }
