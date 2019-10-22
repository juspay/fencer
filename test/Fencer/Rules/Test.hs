{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test
  ( test_rulesLoadRulesYaml
  , test_rulesLoadRulesNonYaml
  , test_rulesLoadRulesRecursively
  )
where

import           BasePrelude

import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           NeatInterpolation (text)
import qualified System.IO.Temp as Temp
import           System.FilePath ((</>))
import           System.Directory (createDirectoryIfMissing)
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           Fencer.Rules
import           Fencer.Types


-- | Test that 'loadRulesFromDirectory' loads rules from YAML files.
test_rulesLoadRulesYaml :: TestTree
test_rulesLoadRulesYaml =
  testCase "Rules are loaded from YAML files" $ do
    Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
      TIO.writeFile (tempDir </> "config1.yml") domain1Text
      TIO.writeFile (tempDir </> "config2.yaml") domain2Text
      definitions <-
        loadRulesFromDirectory (#directory tempDir) (#ignoreDotFiles True)
      assertEqual "unexpected definitions"
        (sortOn domainDefinitionId [domain1, domain2])
        (sortOn domainDefinitionId definitions)

-- | Test that 'loadRulesFromDirectory' loads rules from all files, not just
-- YAML files.
--
-- This counterintuitive behavior matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesNonYaml :: TestTree
test_rulesLoadRulesNonYaml =
  testCase "Rules are loaded from non-YAML files" $ do
    Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
      TIO.writeFile (tempDir </> "config1.bin") domain1Text
      TIO.writeFile (tempDir </> "config2") domain2Text
      definitions <-
        loadRulesFromDirectory (#directory tempDir) (#ignoreDotFiles True)
      assertEqual "unexpected definitions"
        (sortOn domainDefinitionId [domain1, domain2])
        (sortOn domainDefinitionId definitions)

-- | Test that 'loadRulesFromDirectory' loads rules recursively.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesRecursively :: TestTree
test_rulesLoadRulesRecursively =
  testCase "Rules are loaded recursively" $ do
    Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
      createDirectoryIfMissing True (tempDir </> "domain1")
      TIO.writeFile (tempDir </> "domain1/config.yml") domain1Text
      createDirectoryIfMissing True (tempDir </> "domain2/config")
      TIO.writeFile (tempDir </> "domain2/config/config.yml") domain2Text
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
