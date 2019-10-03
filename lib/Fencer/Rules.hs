{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Working with rate limiting rules.
module Fencer.Rules
    ( loadRulesFromDirectory
    , definitionsToRuleTree
    , applyRules
    )
where

import BasePrelude

import qualified Data.HashMap.Strict as HM
import Named ((:!), arg)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName)
import qualified Data.Yaml as Yaml

import Fencer.Types

-- | Gather rate limiting rules (*.yml, *.yaml) from a directory.
-- Subdirectories are not included.
--
-- Throws an exception for unparseable or unreadable files.
loadRulesFromDirectory
    :: "directory" :! FilePath
    -> "ignoreDotFiles" :! Bool
    -> IO [DomainDefinition]
loadRulesFromDirectory
    (arg #directory -> directory)
    (arg #ignoreDotFiles -> ignoreDotFiles)
    =
    do
    files <-
        filterM doesFileExist . map (directory </>) =<<
        listDirectory directory
    let ruleFiles =
            (if ignoreDotFiles then filter (not . isDotFile) else id) $
            filter isYaml files
    mapM Yaml.decodeFileThrow ruleFiles
    -- TODO: what does lyft/ratelimit do with unparseable files?
  where
    isYaml :: FilePath -> Bool
    isYaml file = takeExtension file `elem` [".yml", ".yaml"]

    isDotFile :: FilePath -> Bool
    isDotFile file = "." `isPrefixOf` takeFileName file

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
