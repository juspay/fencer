{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Fencer.Logic".
module Fencer.Logic.Test (tests) where

import           BasePrelude

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, testCase)

import           Fencer.Counter (CounterStatus, counterRemainingLimit)
import           Fencer.Logic (AppState, setRules, updateLimitCounter)
import           Fencer.Rules (definitionsToRuleTree)
import           Fencer.Server.Test (withServer, serverAppState)
import           Fencer.Types


tests :: TestTree
tests = testGroup "Logic tests" [test_logicLimitUnitChange]

-- | Test that a rule limit unit change adds a new counter and leaves
-- the old one intact.
test_logicLimitUnitChange :: TestTree
test_logicLimitUnitChange =
  withServer $ \serverIO ->
    testCase "A rule limit unit change on rule reloading" $ do
      state <- serverAppState <$> serverIO
      void $ atomically $ setRules state (mapRuleDefs definitions1)

      -- Record a hit and get the remaining limit
      st1 <- getRemainingLimit <$> makeAHit state
      assertEqual
        "The remaining rate limit was not updated!"
        (limit - hits)
        st1

      -- Set the new rules and the rules reloaded flag
      atomically $ setRules state (mapRuleDefs definitions2)
      -- Record a hit and get a remaining limit
      st2 <- getRemainingLimit <$> makeAHit state
      assertEqual
        "The remaining rate was affected by a different counter!"
        (limit - hits)
        st2

      -- Set the old rules again
      void $ atomically $ setRules state (mapRuleDefs definitions1)
      -- Record a hit and get a remaining limit
      st1' <- getRemainingLimit <$> makeAHit state
      assertEqual
        "The old counter did not persist!"
        (st1 - hits)
        st1'
 where
  getRemainingLimit :: Maybe (RateLimit, CounterStatus) -> Word
  getRemainingLimit = counterRemainingLimit . snd . fromMaybe (error "")

  makeAHit :: AppState -> IO (Maybe (RateLimit, CounterStatus))
  makeAHit st = atomically $
    updateLimitCounter st (#hits hits) domainId ruleList

  mapRuleDefs :: [DomainDefinition] -> [(DomainId, RuleTree)]
  mapRuleDefs defs =
    [ ( domainDefinitionId rule
      , definitionsToRuleTree $ domainDefinitionDescriptors rule )
    | rule <- defs
    ]

  -- rate limit and hits in the test
  limit = 4 :: Word
  hits  = 1 :: Word

  ruleKey   = RuleKey   "generic_key"
  ruleValue = RuleValue "dream11_order_create"
  ruleList  = [(ruleKey, ruleValue)]
  domainId  = DomainId  "merchant_rate_limits"

  descriptor :: DescriptorDefinition
  descriptor = DescriptorDefinition
        { descriptorDefinitionKey         = ruleKey
        , descriptorDefinitionValue       = Just ruleValue
        , descriptorDefinitionRateLimit   = Just $ RateLimit Minute limit
        , descriptorDefinitionDescriptors = Nothing
        }

  definition1 :: DomainDefinition
  definition1 = DomainDefinition
    { domainDefinitionId          = domainId
    , domainDefinitionDescriptors = [descriptor]
    }

  definitions1 :: [DomainDefinition]
  definitions1 = [definition1]

  definition2 = definition1 {
    domainDefinitionDescriptors =
      [descriptor
        { descriptorDefinitionRateLimit = Just $ RateLimit Hour limit }
      ]
    }

  definitions2 :: [DomainDefinition]
  definitions2 = [definition2]
