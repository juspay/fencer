{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Logic for matching rate limiting requests to rules.
module Ratelimit.Match
    ( -- * Rule trees
      RuleTree
    , RuleBranch(..)
    , makeRuleTree

      -- * Matching
    , matchRequest
    )
where

import BasePrelude
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Named

import Ratelimit.Types

----------------------------------------------------------------------------
-- Rule trees
----------------------------------------------------------------------------

-- | The type for a tree of rules. It is equivalent to a list of
-- 'DescriptorDefinition's, but uses nested hashmaps.
type RuleTree = HashMap (RuleKey, Maybe RuleValue) RuleBranch

data RuleBranch = RuleBranch
    { ruleBranchRateLimit :: !(Maybe RateLimit)
    , ruleBranchNested :: !RuleTree
    }

-- | Convert a list of descriptors to a 'RuleTree'.
makeRuleTree :: [DescriptorDefinition] -> RuleTree
makeRuleTree = HM.fromList . map (\d -> (makeKey d, makeBranch d))
  where
    makeKey :: DescriptorDefinition -> (RuleKey, Maybe RuleValue)
    makeKey desc = (descriptorDefinitionKey desc, descriptorDefinitionValue desc)

    makeBranch :: DescriptorDefinition -> RuleBranch
    makeBranch desc = RuleBranch
        { ruleBranchRateLimit =
              descriptorDefinitionRateLimit desc
        , ruleBranchNested =
              makeRuleTree (fromMaybe [] (descriptorDefinitionDescriptors desc))
        }

----------------------------------------------------------------------------
-- Matching
----------------------------------------------------------------------------

-- | Find the right 'RateLimit' for a descriptor in the 'RuleTree'.
matchRequest :: [(RuleKey, RuleValue)] -> RuleTree -> Maybe RateLimit
matchRequest [] tree =
    Nothing
matchRequest ((key, value):rest) tree = do
    -- Try matching the more specific rule (key, value) first. If it's not
    -- found, match the wildcard (key, _) rule. If neither is found,
    -- 'matchRequest' fails immediately.
    branch <-
        HM.lookup (key, Just value) tree <|>
        HM.lookup (key, Nothing) tree
    -- If we reached the end of the descriptor, we use the current rate
    -- limit. Otherwise we keep going.
    if null rest
        then ruleBranchRateLimit branch
        else matchRequest rest (ruleBranchNested branch)
