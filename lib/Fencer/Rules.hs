{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Working with rate limiting rules.
module Fencer.Rules
    ( LoadRulesError(..)
    , showErrors
    , loadRulesFromDirectory
    , definitionsToRuleTree
    , applyRules
    )
where

import BasePrelude

import Control.Applicative (liftA2)
import Control.Monad.Extra (partitionM, concatMapM)
import Data.Either.Combinators (mapLeft)
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import Named ((:!), arg)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, pathIsSymbolicLink)
import System.FilePath ((</>), makeRelative, normalise, splitDirectories)
import qualified Data.Yaml as Yaml

import Fencer.Types

data LoadRulesError
  = LoadRulesParseError FilePath Yaml.ParseException
  | LoadRulesIOError IOException

instance Show LoadRulesError where
  show (LoadRulesParseError file yamlEx) =
    show file ++ ", " ++ (Yaml.prettyPrintParseException yamlEx)
  show (LoadRulesIOError ex) = "IO error: " ++ show ex

-- | Show a list of 'LoadRulesError's.
showErrors :: [LoadRulesError] -> String
showErrors = join . intersperse ", " . fmap show


-- | Read rate limiting rules from a directory, recursively. Files are
-- assumed to be YAML, but do not have to have a @.yml@ extension. If
-- any of directories below, including the main sub-directory, starts
-- with a dot and dot-files are ignored, this function will skip
-- loading rules from it and all directories below it.
--
-- In case of unparsable or unreadable files returns a list of
-- exceptions.
loadRulesFromDirectory
    :: "rootDirectory" :! FilePath
    -> "subDirectory" :! FilePath
    -> "ignoreDotFiles" :! Bool
    -> IO (Either [LoadRulesError] [DomainDefinition])
loadRulesFromDirectory
    (arg #rootDirectory -> rootDirectory)
    (arg #subDirectory -> subDirectory)
    (arg #ignoreDotFiles -> ignoreDotFiles)
    =
    do
    let directory = rootDirectory </> subDirectory
    files <- listAllFiles directory
    let filteredFiles = if ignoreDotFiles
        then filter (not . isDotFile) files
        else files
    foldl'
      combine
      (pure . Right $ [])
      filteredFiles
  where
    combine
      :: IO (Either [LoadRulesError] [DomainDefinition])
      -> FilePath
      -> IO (Either [LoadRulesError] [DomainDefinition])
    combine acc file = do
      let
        res = liftBoth <$>
          catch
            ((mapLeft (LoadRulesParseError file)) <$> Yaml.decodeFileEither @DomainDefinition file)
            (pure . Left . LoadRulesIOError)
      liftA2 merge res acc

    liftBoth
      :: Either LoadRulesError DomainDefinition
      -> Either [LoadRulesError] [DomainDefinition]
    liftBoth (Left e)  = Left  [e]
    liftBoth (Right v) = Right [v]

    merge
      :: Either [LoadRulesError] [DomainDefinition]
      -> Either [LoadRulesError] [DomainDefinition]
      -> Either [LoadRulesError] [DomainDefinition]
    merge (Left fs1)  (Left fs2)  = Left $ fs1 ++ fs2
    merge (Left fs1)  (Right _  ) = Left fs1
    merge (Right _  ) (Left fs2)  = Left fs2
    merge (Right ds1) (Right ds2) = Right $ ds1 ++ ds2

    isDotFile :: FilePath -> Bool
    isDotFile file =
      let
        normRelPath = normalise $ makeRelative rootDirectory file
      in any ("." `isPrefixOf`) $ splitDirectories normRelPath

    -- | Is the path a true directory (not a symlink)?
    isDirectory :: FilePath -> IO Bool
    isDirectory dir =
        liftA2 (&&)
            (doesDirectoryExist dir)
            (not <$> (pathIsSymbolicLink dir `catchIOError` \_ -> pure False))

    -- | List all files in a directory, recursively, without following
    -- symlinks.
    listAllFiles :: FilePath -> IO [FilePath]
    listAllFiles dir = do
        -- TODO: log exceptions
        contents <-
            map (dir </>) <$>
            (listDirectory dir `catchIOError` \_ -> pure [])
        -- files = normal files and links to files
        -- dirs = directories, but not links to directories
        (files, other) <- partitionM doesFileExist contents
        dirs <- filterM isDirectory other
        (files ++) <$> concatMapM listAllFiles dirs

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
