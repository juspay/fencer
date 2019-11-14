{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Working with rate limiting rules.
module Fencer.Rules
    ( LoadRulesError(..)
    , prettyPrintErrors
    , loadRulesFromDirectory
    , definitionsToRuleTree
    , domainToRuleTree
    , applyRules
    )
where

import BasePrelude

import Control.Applicative (liftA2)
import Control.Monad.Extra (partitionM, concatMapM)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NE
import Named ((:!), arg)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, pathIsSymbolicLink)
import System.FilePath ((</>), makeRelative, normalise, splitDirectories)
import qualified Data.Yaml as Yaml

import Fencer.Types

data LoadRulesError
  = LoadRulesParseError FilePath Yaml.ParseException
  | LoadRulesIOError IOException
  | LoadRulesDuplicateDomain DomainId
  deriving stock (Show)

-- | Pretty-print a list of 'LoadRulesError's.
prettyPrintErrors :: [LoadRulesError] -> String
prettyPrintErrors = intercalate ", " . fmap showError
  where
    showError (LoadRulesParseError file yamlEx) =
      show file ++ ", " ++ (Yaml.prettyPrintParseException yamlEx)
    showError (LoadRulesIOError ex) =
      "IO error: " ++ displayException ex
    showError (LoadRulesDuplicateDomain d) =
      "duplicate domain " ++ (show . unDomainId $ d) ++ " in config file"

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
    finalChecks <$> mapM loadFile filteredFiles
  where
    loadFile :: FilePath -> IO (Either LoadRulesError (Maybe DomainDefinition))
    loadFile file = catch
      (parseErrorHandle file <$> Yaml.decodeFileEither @DomainDefinition file)
      (pure . Left . LoadRulesIOError)

    -- | Handle a special case when the input file cannot be read due
    -- to file permissions by returning Nothing on the Right.
    parseErrorHandle
      :: FilePath
      -> Either Yaml.ParseException DomainDefinition
         ----------------------------------------------
      -> Either LoadRulesError (Maybe DomainDefinition)
    parseErrorHandle _    (Right def)  = Right $ Just def
    parseErrorHandle file (Left parEx) = case parEx of
      Yaml.InvalidYaml (Just (Yaml.YamlException _)) ->
        Right Nothing
      err ->
        Left $ LoadRulesParseError file err

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

    -- | Perform final checks to make sure the behavior matches that
    -- of @lyft/ratelimit@.
    finalChecks
      :: [Either LoadRulesError (Maybe DomainDefinition)]
      -> Either [LoadRulesError] [DomainDefinition]
    finalChecks res = case partitionEithers res of
      (errs@(_:_), _    ) -> Left errs
      ([]        , mRules) -> do
        -- check if there are any duplicate domains
        let
          rules = catMaybes mRules
          groupedRules :: [NonEmpty DomainDefinition] = NE.groupBy
            ((==) `on` (unDomainId . domainDefinitionId))
            (NE.fromList $ sortOn domainDefinitionId rules)
        if (length @[] rules /= length @[] groupedRules)
          then
            let dupDomain = NE.head . head $ filter (\l -> NE.length l > 1) groupedRules
            in Left . pure . LoadRulesDuplicateDomain . domainDefinitionId $ dupDomain
          else Right rules

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

-- | Convert a domain to a 'RuleTree' together with the domain ID. This is a
-- trivial function but we need it often.
domainToRuleTree :: DomainDefinition -> (DomainId, RuleTree)
domainToRuleTree domain =
  ( domainDefinitionId domain
  , definitionsToRuleTree (domainDefinitionDescriptors domain)
  )

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
