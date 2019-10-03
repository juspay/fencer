{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test
  ( test_loadRulesYaml
  )
where

import           BasePrelude

import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)
import qualified System.IO.Temp as Temp
import           NeatInterpolation (text)
import           System.FilePath ((</>))
import           Data.List (sortOn)

import           Fencer.Types
import           Fencer.Rules

-- | Test that 'loadRulesFromDirectory' loads rules from YAML files.
test_loadRulesYaml :: TestTree
test_loadRulesYaml =
  testCase "Rules are loaded from YAML files" $ do
    Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
      TIO.writeFile (tempDir </> "config1.yml") domain1Text
      TIO.writeFile (tempDir </> "config2.yaml") domain2Text
      definitions <-
        loadRulesFromDirectory (#directory tempDir) (#ignoreDotFiles True)
      assertEqual "unexpected definitions"
        (sortOn domainDefinitionId [domain1, domain2])
        (sortOn domainDefinitionId definitions)

----------------------------------------------------------------------------
-- Sample definitions
----------------------------------------------------------------------------

domain1 :: DomainDefinition
domain1 = DomainDefinition
  { domainDefinitionId = DomainId "domain1"
  , domainDefinitionDescriptors = descriptor1 :| []
  }
  where
    descriptor1 :: DescriptorDefinition
    descriptor1 = DescriptorDefinition
      { descriptorDefinitionKey = RuleKey "some key"
      , descriptorDefinitionValue = Just $ RuleValue "some value"
      , descriptorDefinitionRateLimit = Nothing
      , descriptorDefinitionDescriptors = Nothing
      }

domain1Text :: Text
domain1Text = [text|
  domain: domain1
  descriptors:
    - key: some key
      value: some value
  |]

domain2 :: DomainDefinition
domain2 = DomainDefinition
  { domainDefinitionId = DomainId "domain2"
  , domainDefinitionDescriptors = descriptor2 :| []
  }
  where
    descriptor2 :: DescriptorDefinition
    descriptor2 = DescriptorDefinition
      { descriptorDefinitionKey = RuleKey "some key 2"
      , descriptorDefinitionValue = Nothing
      , descriptorDefinitionRateLimit = Nothing
      , descriptorDefinitionDescriptors = Nothing
      }

domain2Text :: Text
domain2Text = [text|
  domain: domain2
  descriptors:
    - key: some key 2
  |]
