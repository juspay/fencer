{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Tests for "Fencer.Rules".
module Fencer.Rules.Test
  ( test_rulesLoadRulesYaml
  , test_rulesLoadRulesNonYaml
  , test_rulesLoadRulesRecursively
  , test_rulesLimitUnitChange
  )
where

import           BasePrelude

import qualified Data.HashMap.Strict as HM
import           Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           NeatInterpolation (text)
import qualified StmContainers.Map as StmMap
import qualified System.IO.Temp as Temp
import           System.FilePath ((</>))
import           System.Directory (createDirectoryIfMissing)
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import           Fencer.AppState (appStateCounters, appStateRules, recordHits, setRules)
import           Fencer.Counter (CounterKey(..), counterHits)
import           Fencer.Rules
import           Fencer.Types

import           Fencer.Server.Test (withServer, serverAppState)


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

-- | Test that a rule limit unit change adds a new counter and leaves
-- the old one intact.
test_rulesLimitUnitChange :: TestTree
test_rulesLimitUnitChange =
  withServer $ \serverIO ->
    testCase "A rule limit unit change on rule reloading" $ do
      Temp.withSystemTempDirectory "fencer-config-unit" $ \tempDir -> do
        createDirectoryIfMissing True (tempDir </> dir)

        definitions1 <- writeLoad tempDir merchantLimitsText1
        state <- serverAppState <$> serverIO

        atomically $ setRules state (mapRuleDefs definitions1)

        ruleTree :: RuleTree <- atomically $
          fromMaybe' <$> StmMap.lookup domainId (appStateRules state)
        let ruleBranch = fromMaybe' $ HM.lookup (ruleKey, Just ruleValue) ruleTree
        let rateLimit =  fromMaybe' $ ruleBranchRateLimit ruleBranch

        -- Record a hit
        void $ atomically $ recordHits state (#hits 1) (#limit rateLimit) counterKey1

        mV1 <- atomically $ StmMap.lookup counterKey1 $ appStateCounters state

        -- Change rules in the configuration
        definitions2 <- writeLoad tempDir merchantLimitsText2

        -- Set the new rules and the rules reloaded flag
        atomically $ setRules state (mapRuleDefs definitions2)

        mV1' <- atomically $ StmMap.lookup counterKey1 $ appStateCounters state
        mV2  <- atomically $ StmMap.lookup counterKey2 $ appStateCounters state

        assertBool
          "The original counter was not updated after recording a hit!"
          ((counterHits <$> mV1) == Just 1)
        assertBool
          "The original counter was mistakenly updated in the meantime!"
          (mV1 == mV1')
        assertBool "The secondary counter was set!" (mV2 == Nothing)
 where
  mapRuleDefs :: [DomainDefinition] -> [(DomainId, RuleTree)]
  mapRuleDefs defs =
    [ ( domainDefinitionId rule
      , definitionsToRuleTree (NE.toList . domainDefinitionDescriptors $ rule))
    | rule <- defs
    ]

  dir     = "d11-ratelimits"
  cfgFile = "d11-ratelimits1.yaml"

  writeLoad :: FilePath -> Text -> IO [DomainDefinition]
  writeLoad tempDir txt = do
    TIO.writeFile (tempDir </> dir </> cfgFile) txt
    loadRulesFromDirectory (#directory tempDir) (#ignoreDotFiles True)

  ruleKey   = RuleKey   "generic_key"
  ruleValue = RuleValue "dream11_order_create"
  domainId  = DomainId  "merchant_rate_limits"

  counterKey1 = CounterKey
    { counterKeyDomain     = domainId
    , counterKeyDescriptor = [ (ruleKey, ruleValue) ]
    , counterKeyUnit       = Minute }

  counterKey2 :: CounterKey
  counterKey2 = counterKey1 { counterKeyUnit = Hour }

  fromMaybe' :: Maybe a -> a
  fromMaybe' = fromMaybe (error "")


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

merchantLimitsText1 :: Text
merchantLimitsText1 = [text|
  domain: merchant_rate_limits
  descriptors:
  - key: generic_key
    value: dream11_order_create
    rate_limit:
      unit: minute
      requests_per_unit: 400000
  |]

merchantLimitsText2 :: Text
merchantLimitsText2 = [text|
  domain: merchant_rate_limits
  descriptors:
  - key: generic_key
    value: dream11_order_create
    rate_limit:
      unit: hour
      requests_per_unit: 400000
  |]
