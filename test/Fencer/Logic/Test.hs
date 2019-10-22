{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests for "Fencer.Logic".
module Fencer.Logic.Test
  ( test_logicLimitUnitChange
  ) where

import           BasePrelude

import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           NeatInterpolation (text)
import qualified StmContainers.Map as StmMap
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import qualified System.IO.Temp as Temp
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertBool, testCase)

import           Fencer.Counter (CounterKey(..), counterHits)
import           Fencer.Logic (appStateCounters, setRules, updateLimitCounter)
import           Fencer.Rules (definitionsToRuleTree, loadRulesFromDirectory)
import           Fencer.Server.Test (withServer, serverAppState)
import           Fencer.Types


-- | Test that a rule limit unit change adds a new counter and leaves
-- the old one intact.
test_logicLimitUnitChange :: TestTree
test_logicLimitUnitChange =
  withServer $ \serverIO ->
    testCase "A rule limit unit change on rule reloading" $ do
      Temp.withSystemTempDirectory "fencer-config-unit" $ \tempDir -> do
        createDirectoryIfMissing True (tempDir </> dir)

        definitions1 <- writeLoad tempDir merchantLimitsText1
        state <- serverAppState <$> serverIO

        void $ atomically $ setRules state (mapRuleDefs definitions1)

        -- Record a hit
        void $ atomically $ updateLimitCounter state (#hits 1) domainId [(ruleKey, ruleValue)]

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


----------------------------------------------------------------------------
-- Sample definitions
----------------------------------------------------------------------------

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
