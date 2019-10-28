{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test (tests) where

import           BasePrelude

import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Named ((:!), arg)
import           NeatInterpolation (text)
import qualified System.IO.Temp as Temp
import           System.FilePath (splitFileName, (</>))
import           System.Directory (createDirectoryIfMissing)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, Assertion, testCase)

import           Fencer.Rules
import           Fencer.Types


tests :: TestTree
tests = testGroup "Rule tests"
  [ test_rulesLoadRulesYaml
  , test_rulesLoadRulesNonYaml
  , test_rulesLoadRulesRecursively
  , test_rulesLoadRulesDotDirectory
  , test_rulesLoadRulesRUNTIME_IGNOREDOTFILES
  ]

-- | Create given directory structure and check that 'loadRulesFromDirectory'
-- produces expected result.
expectLoadRules
  :: "ignoreDotFiles" :! Bool
  -> "files" :! [(FilePath, Text)]
  -> "result" :! [DomainDefinition]
  -> Assertion
expectLoadRules
  (arg #ignoreDotFiles -> ignoreDotFiles)
  (arg #files -> files)
  (arg #result -> result) =
  Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
    forM_ files $ \(path, txt) -> do
      let (dir, file) = splitFileName path
      createDirectoryIfMissing True (tempDir </> dir)
      TIO.writeFile (tempDir </> dir </> file) txt
    definitions <- loadRulesFromDirectory
      (#directory tempDir)
      (#ignoreDotFiles ignoreDotFiles)
    assertEqual "unexpected definitions"
      (sortOn domainDefinitionId result)
      (sortOn domainDefinitionId definitions)

-- | test that 'loadRulesFromDirectory' loads rules from YAML files.
test_rulesLoadRulesYaml :: TestTree
test_rulesLoadRulesYaml =
  testCase "Rules are loaded from YAML files" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ ("config1.yml", domain1Text)
        , ("config2.yaml", domain2Text) ]
      )
      (#result [domain1, domain2])

-- | test that 'loadRulesFromDirectory' loads rules from a
-- dot-directory when dot-files should be ignored.
test_rulesLoadRulesDotDirectory :: TestTree
test_rulesLoadRulesDotDirectory =
  testCase "Rules are loaded from a dot-directory" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ (".domain1/config1.yml", domain1Text)
        , (".domain2/config2.yaml", domain2Text) ]
      )
      (#result [domain1, domain2])

-- | test that 'loadRulesFromDirectory' respects the
-- RUNTIME_IGNOREDOTFILES environment variable.
test_rulesLoadRulesRUNTIME_IGNOREDOTFILES :: TestTree
test_rulesLoadRulesRUNTIME_IGNOREDOTFILES =
  testCase "Rules are not loaded from a dot-file" $ do
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ ("config1.yml", domain1Text)
        , ("dir/.config2.yaml", domain2Text) ]
      )
      (#result [domain1])

-- | Test that 'loadRulesFromDirectory' loads rules from all files, not just
-- YAML files.
--
-- This counterintuitive behavior matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesNonYaml :: TestTree
test_rulesLoadRulesNonYaml =
  testCase "Rules are loaded from non-YAML files" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ ("config1.bin", domain1Text)
        , ("config2", domain2Text) ]
      )
      (#result [domain1, domain2])

-- | Test that 'loadRulesFromDirectory' loads rules recursively.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesRecursively :: TestTree
test_rulesLoadRulesRecursively =
  testCase "Rules are loaded recursively" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ ("domain1/config.yml", domain1Text)
        , ("domain2/config/config.yml", domain2Text) ]
      )
      (#result [domain1, domain2])

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
