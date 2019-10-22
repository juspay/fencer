{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Tests for "Fencer.Logic".
module Fencer.Logic.Test
  ( test_logicLimitUnitChange
  ) where

import           BasePrelude

import           Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified StmContainers.Map as StmMap
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertBool, testCase)

import           Fencer.Counter (CounterKey(..), counterHits)
import           Fencer.Logic (appStateCounters, setRules, updateLimitCounter)
import           Fencer.Rules (definitionsToRuleTree)
import           Fencer.Server.Test (withServer, serverAppState)
import           Fencer.Types


-- | Test that a rule limit unit change adds a new counter and leaves
-- the old one intact.
test_logicLimitUnitChange :: TestTree
test_logicLimitUnitChange =
  withServer $ \serverIO ->
    testCase "A rule limit unit change on rule reloading" $ do
      state <- serverAppState <$> serverIO
      void $ atomically $ setRules state (mapRuleDefs definitions1)

      -- Record a hit
      void $ atomically $ updateLimitCounter state (#hits 1) domainId [(ruleKey, ruleValue)]
      mV1 <- atomically $ StmMap.lookup counterKey1 $ appStateCounters state

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

  ruleKey   = RuleKey   "generic_key"
  ruleValue = RuleValue "dream11_order_create"
  domainId  = DomainId  "merchant_rate_limits"

  counterKey1 = CounterKey
    { counterKeyDomain     = domainId
    , counterKeyDescriptor = [ (ruleKey, ruleValue) ]
    , counterKeyUnit       = Minute }

  counterKey2 :: CounterKey
  counterKey2 = counterKey1 { counterKeyUnit = Hour }

  descriptor :: DescriptorDefinition
  descriptor = DescriptorDefinition
        { descriptorDefinitionKey         = ruleKey
        , descriptorDefinitionValue       = Just ruleValue
        , descriptorDefinitionRateLimit   = Just $ RateLimit Minute 4
        , descriptorDefinitionDescriptors = Nothing
        }

  definition1 :: DomainDefinition
  definition1 = DomainDefinition
    { domainDefinitionId          = domainId
    , domainDefinitionDescriptors = descriptor :| []
    }

  definitions1 :: [DomainDefinition]
  definitions1 = [definition1]

  definition2 = definition1 {
    domainDefinitionDescriptors =
      (descriptor
        { descriptorDefinitionRateLimit = Just $ RateLimit Hour 4 }
      ) :| []
    }

  definitions2 :: [DomainDefinition]
  definitions2 = [definition2]
