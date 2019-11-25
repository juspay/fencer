{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test
  ( tests
  , writeAndLoadRules
  -- example values
  , domain1Text
  , domain2Text
  , RuleFile(..)
  , simpleRuleFile
  ) where

import           BasePrelude

import           Data.List (sortOn)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import           Named ((:!), arg)
import           NeatInterpolation (text)
import qualified System.IO.Temp as Temp
import           System.FilePath (takeDirectory, (</>))
import qualified System.Directory as Dir
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, assertEqual, Assertion, testCase)

import           Fencer.Rules
import           Fencer.Types


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
        [ simpleRuleFile "config1.yml" domain1Text
        , simpleRuleFile "config2.yaml" domain2Text ]
      )
      (#result $ Right [domain1, domain2])

-- | test that 'loadRulesFromDirectory' does not load rules from a
-- dot-directory when dot-files should be ignored.
test_rulesLoadRulesDotDirectory :: TestTree
test_rulesLoadRulesDotDirectory =
  testCase "Rules are not loaded from a dot-directory" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile (".domain1" </> "config1.yml") domain1Text
        , simpleRuleFile ("domain2" </> "config2.yaml") domain2Text ]
      )
      (#result $ Right [domain2])

-- | test that 'loadRulesFromDirectory' ignores dot-files.
test_rulesLoadRules_ignoreDotFiles :: TestTree
test_rulesLoadRules_ignoreDotFiles =
  testCase "Rules are not loaded from a dot-file" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile "config1.yml" domain1Text
        , simpleRuleFile ("dir" </> ".config2.yaml") domain2Text ]
      )
      (#result $ Right [domain1])

-- | test that 'loadRulesFromDirectory' does not ignore dot files.
test_rulesLoadRules_dontIgnoreDotFiles :: TestTree
test_rulesLoadRules_dontIgnoreDotFiles =
  testCase "Rules are loaded from a dot-file" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files
        [ simpleRuleFile "config1.yml" domain1Text
        , simpleRuleFile ("dir" </> ".config2.yaml") domain2Text ]
      )
      (#result $ Right [domain1, domain2])

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
        [ simpleRuleFile "config1.bin" domain1Text
        , simpleRuleFile "config2" domain2Text ]
      )
      (#result $ Right [domain1, domain2])

-- | Test that 'loadRulesFromDirectory' loads rules recursively.
--
-- This matches the behavior of @lyft/ratelimit@.
test_rulesLoadRulesRecursively :: TestTree
test_rulesLoadRulesRecursively =
  testCase "Rules are loaded recursively" $
    expectLoadRules
      (#ignoreDotFiles True)
      (#files
        [ simpleRuleFile ("domain1" </> "config.yml") domain1Text
        , simpleRuleFile
            ("domain2" </> "config" </> "config.yml")
            domain2Text
        ]
      )
      (#result $ Right [domain1, domain2])

-- | Test that 'loadRulesFromDirectory' returns exceptions for an
-- invalid domain. The 'loadRulesFromDirectory' function fails to load
-- any rules in presence of at least one invalid domain.
test_rulesLoadRulesException :: TestTree
test_rulesLoadRulesException =
  testCase "Rules fail to load for an invalid domain" $
    expectLoadRules
      (#ignoreDotFiles False)
      (#files
        [ simpleRuleFile "domain1.yaml" domain1Text
        , simpleRuleFile "faultyDomain.yaml" faultyDomain
        ]
      )
      (#result $ Left
         [LoadRulesParseError "faultyDomain.yaml" $ Yaml.AesonException ""])

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
      (#result $ Right [domain2])
 where
  file1, file2 :: RuleFile
  file1 = MkRuleFile
    ("domain1" </> "config.yml")
    domain1Text
    (const Dir.emptyPermissions)
  file2 = simpleRuleFile
    ("domain2" </> "config" </> "config.yml")
    domain2Text

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Get a list of values on the Left or an empty list if it is a
-- Right value.
toErrorList :: Either [a] [b] -> [a]
toErrorList (Right _) = []
toErrorList (Left xs) = xs

-- | A record useful in testing, which groups together a file path,
-- its contents and file permissions.
data RuleFile = MkRuleFile
  {  -- | The path to the file
    ruleFilePath :: FilePath
    -- | The contents of the file in plain text
  , ruleFileContents :: Text
    -- | A function specifying how the file permissions should be
    -- changed, i.e., what they should be once the file is written to
    -- disk.
  , ruleFileModifyPermissions :: Dir.Permissions -> Dir.Permissions
  }

simpleRuleFile :: FilePath -> Text -> RuleFile
simpleRuleFile p c = MkRuleFile p c id

-- | Write contents to a path in the given root and modify file
-- permissions.
writeFile
  :: "root" :! FilePath
  -> "file" :! RuleFile
  -> IO ()
writeFile
  (arg #root -> root)
  (arg #file -> file) = do

  let
    dir = takeDirectory (ruleFilePath file)
    fullPath = root </> (ruleFilePath file)
  Dir.createDirectoryIfMissing True (root </> dir)
  TIO.writeFile fullPath (ruleFileContents file)
  perms <- Dir.getPermissions fullPath
  Dir.setPermissions fullPath (ruleFileModifyPermissions file perms)

-- | Write the content of files at the given root and load the files.
writeAndLoadRules
  :: "ignoreDotFiles" :! Bool
  -> "root" :! FilePath
  -> "files" :! [RuleFile]
  -> IO (Either [LoadRulesError] [DomainDefinition])
writeAndLoadRules
  (arg #ignoreDotFiles -> ignoreDotFiles)
  (arg #root -> root)
  (arg #files -> files) = do

  forM_ files $ \file -> Fencer.Rules.Test.writeFile
    (#root root)
    (#file file)
  loadRulesFromDirectory
    (#rootDirectory root)
    (#subDirectory ".")
    (#ignoreDotFiles ignoreDotFiles)

-- | Create given directory structure and check that
-- 'loadRulesFromDirectory' produces expected result such that file
-- permissions are configurable.
expectLoadRules
  :: "ignoreDotFiles" :! Bool
  -> "files" :! [RuleFile]
  -> "result" :! Either [LoadRulesError] [DomainDefinition]
  -> Assertion
expectLoadRules
  (arg #ignoreDotFiles -> ignoreDotFiles)
  (arg #files -> files)
  (arg #result -> result) =
  Temp.withSystemTempDirectory "fencer-config" $ \tempDir ->
    writeAndLoadRules
      (#ignoreDotFiles ignoreDotFiles)
      (#root tempDir)
      (#files files)
      >>= \case
      f@(Left _) ->
        -- Paths to temporary files vary and there is not much point
        -- in writing down exact expected exception messages so the
        -- only assertion made is that the number of exceptions is the
        -- same.
        assertEqual
          "unexpected failure"
          (length . toErrorList $ result)
          (length . toErrorList $ f)
      Right definitions -> assertBool "unexpected definitions"
        (((==) `on` show)
        (sortOn domainDefinitionId <$> result)
        (Right $ sortOn domainDefinitionId definitions))

----------------------------------------------------------------------------
-- Sample definitions
----------------------------------------------------------------------------

descriptor1 :: DescriptorDefinition
descriptor1 = DescriptorDefinition
  { descriptorDefinitionKey = RuleKey "some key"
  , descriptorDefinitionValue = Just $ RuleValue "some value"
  , descriptorDefinitionRateLimit = Nothing
  , descriptorDefinitionDescriptors = Nothing
  }

descriptor2 :: DescriptorDefinition
descriptor2 = DescriptorDefinition
  { descriptorDefinitionKey = RuleKey "some key 2"
  , descriptorDefinitionValue = Nothing
  , descriptorDefinitionRateLimit = Nothing
  , descriptorDefinitionDescriptors = Nothing
  }

domain1 :: DomainDefinition
domain1 = DomainDefinition
  { domainDefinitionId = DomainId "domain1"
  , domainDefinitionDescriptors = [descriptor1]
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
  , domainDefinitionDescriptors = [descriptor2]
  }

domain2Text :: Text
domain2Text = [text|
  domain: domain2
  descriptors:
    - key: some key 2
  |]

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

minimalDomain :: DomainDefinition
minimalDomain = DomainDefinition
  { domainDefinitionId = DomainId "min"
  , domainDefinitionDescriptors = []
  }

minimalDomainText :: Text
minimalDomainText = [text| domain: min |]

separatorDomainText :: Text
separatorDomainText = [text|
  ---
  domain: another
  descriptors:
    - key: some key
      value: some value
    - key: some key 2
  |]

separatorDomain :: DomainDefinition
separatorDomain = DomainDefinition
  { domainDefinitionId = DomainId "another"
  , domainDefinitionDescriptors = [descriptor1, descriptor2]
  }
