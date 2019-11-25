{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test
  ( tests
  ) where

import           BasePrelude

import qualified Data.Yaml as Yaml
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase)

import           Fencer.Rules
import           Fencer.Rules.Test.Examples
import           Fencer.Rules.Test.Helpers (expectLoadRules)
import           Fencer.Rules.Test.Types


tests :: TestTree
tests = testGroup "Rule tests"
  [ test_rulesLoadRulesYaml
  , test_rulesLoadRulesNonYaml
  , test_rulesLoadRulesRecursively
  , test_rulesLoadRulesDotDirectory
  , test_rulesLoadRules_ignoreDotFiles
  , test_rulesLoadRules_dontIgnoreDotFiles
  , test_rulesLoadRulesException
  , test_rulesLoadRulesMinimal
  , test_rulesYAMLSeparator
  , test_rulesLoadRulesReadPermissions
  ]

-- | test that 'loadRulesFromDirectory' loads rules from YAML files.
test_rulesLoadRulesYaml :: TestTree
test_rulesLoadRulesYaml =
  testCase "Rules are loaded from YAML files" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile "config1.yml" domainDescriptorKeyValueText
        , simpleRuleFile "config2.yaml" domainDescriptorKeyText ]
      )
      (#result $ Right [domainDescriptorKeyValue, domainDescriptorKey])

-- | test that 'loadRulesFromDirectory' does not load rules from a
-- dot-directory when dot-files should be ignored.
test_rulesLoadRulesDotDirectory :: TestTree
test_rulesLoadRulesDotDirectory =
  testCase "Rules are not loaded from a dot-directory" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile
            (".domain1" </> "config1.yml")
            domainDescriptorKeyValueText
        , simpleRuleFile
            ("domain2" </> "config2.yaml")
            domainDescriptorKeyText
        ]
      )
      (#result $ Right [domainDescriptorKey])

-- | test that 'loadRulesFromDirectory' ignores dot-files.
test_rulesLoadRules_ignoreDotFiles :: TestTree
test_rulesLoadRules_ignoreDotFiles =
  testCase "Rules are not loaded from a dot-file" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile "config1.yml" domainDescriptorKeyValueText
        , simpleRuleFile ("dir" </> ".config2.yaml") domainDescriptorKeyText ]
      )
      (#result $ Right [domainDescriptorKeyValue])

-- | test that 'loadRulesFromDirectory' does not ignore dot files.
test_rulesLoadRules_dontIgnoreDotFiles :: TestTree
test_rulesLoadRules_dontIgnoreDotFiles =
  testCase "Rules are loaded from a dot-file" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files
        [ simpleRuleFile "config1.yml" domainDescriptorKeyValueText
        , simpleRuleFile ("dir" </> ".config2.yaml") domainDescriptorKeyText ]
      )
      (#result $ Right [domainDescriptorKeyValue, domainDescriptorKey])

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
        [ simpleRuleFile "config1.bin" domainDescriptorKeyValueText
        , simpleRuleFile "config2" domainDescriptorKeyText ]
      )
      (#result $ Right [domainDescriptorKeyValue, domainDescriptorKey])

-- | Test that 'loadRulesFromDirectory' loads rules recursively.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesRecursively :: TestTree
test_rulesLoadRulesRecursively =
  testCase "Rules are loaded recursively" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile
            ("domain1" </> "config.yml")
            domainDescriptorKeyValueText
        , simpleRuleFile
            ("domain2" </> "config" </> "config.yml")
            domainDescriptorKeyText
        ]
      )
      (#result $ Right [domainDescriptorKeyValue, domainDescriptorKey])

-- | Test that 'loadRulesFromDirectory' returns exceptions for an
-- invalid domain. The 'loadRulesFromDirectory' function fails to load
-- any rules in presence of at least one invalid domain.
test_rulesLoadRulesException :: TestTree
test_rulesLoadRulesException =
  testCase "Rules fail to load for an invalid domain" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files
        [ simpleRuleFile "domain1.yaml" domainDescriptorKeyValueText
        , simpleRuleFile "faultyDomain.yaml" faultyDomain
        ]
      )
      (#result $ Left
         [LoadRulesParseError "faultyDomain.yaml" $
           Yaml.AesonException
             "Error in $.descriptors[1]: key \"key\" not present"])

-- | test that 'loadRulesFromDirectory' accepts a minimal
-- configuration containing only the domain id.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesMinimal :: TestTree
test_rulesLoadRulesMinimal =
  testCase "Minimal rules contain domain id only" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files [simpleRuleFile "min.yaml" minimalDomainText])
      (#result $ Right [minimalDomain])

-- | test that 'loadRulesFromDirectory' accepts a configuration that
-- starts in "---", a YAML document separator. Fencer matches
-- Ratelimit in such a case: it works only if there is one YAML
-- document in the file, i.e., one domain. In general, neither
-- Ratelimit nor Fencer support YAML files with multiple
-- documents.
test_rulesYAMLSeparator :: TestTree
test_rulesYAMLSeparator =
  testCase "One domain after a YAML separator" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files [simpleRuleFile "sep.yaml" separatorDomainText] )
      (#result $ Right [separatorDomain])

-- | test that 'loadRulesFromDirectory' loads a configuration file in
-- presence of another configuration file without read permissions.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesReadPermissions :: TestTree
test_rulesLoadRulesReadPermissions =
  testCase "Configuration file read permissions" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files [file1, file2])
      (#result $ Right [domainDescriptorKey])
 where
  file1, file2 :: RuleFile
  file1 = MkRuleFile
    ("domain1" </> "config.yml")
    domainDescriptorKeyValueText
    (const Dir.emptyPermissions)
  file2 = simpleRuleFile
    ("domain2" </> "config" </> "config.yml")
    domainDescriptorKeyText
