{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Working with rate limiting rules.
module Fencer.Rules
    ( definitionsToRuleTree
    , applyRules
    )
where

import BasePrelude

import qualified Data.HashMap.Strict as HM

import Fencer.Types

-- | Convert a list of descriptors to a 'RuleTree'.
definitionsToRuleTree :: [DescriptorDefinition] -> RuleTree
definitionsToRuleTree = HM.fromList . map (\d -> (makeKey d, makeBranch d))
  where
    makeKey :: DescriptorDefinition -> (RuleKey, Maybe RuleValue)
    makeKey desc = (descriptorDefinitionKey desc, descriptorDefinitionValue desc)

    makeBranch :: DescriptorDefinition -> RuleBranch
    makeBranch desc = RuleBranch
        { ruleBranchRateLimit =
              descriptorDefinitionRateLimit desc
        , ruleBranchNested =
              definitionsToRuleTree $
                  fromMaybe [] (descriptorDefinitionDescriptors desc)
        }

-- | In a tree of rules, find the 'RateLimit' that should be applied to a
-- specific descriptor.
applyRules :: [(RuleKey, RuleValue)] -> RuleTree -> Maybe RateLimit
applyRules [] _tree =
    Nothing
applyRules ((key, value):rest) tree = do
    -- Try matching the more specific rule (key, value) first. If it's not
    -- found, match the wildcard (key, _) rule. If neither is found,
    -- 'matchRequest' fails immediately.
    branch <-
        HM.lookup (key, Just value) tree <|>
        HM.lookup (key, Nothing) tree
    -- If we reached the end of the descriptor, we use the current rate
    -- limit. Otherwise we keep going.
    if null @[] rest
        then ruleBranchRateLimit branch
        else applyRules rest (ruleBranchNested branch)
