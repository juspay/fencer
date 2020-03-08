{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- | Working with rate limiting rules.
module Fencer.Rules
    ( LoadRulesError(..)
    , prettyPrintErrors
    , showError
    , loadRulesFromDirectory
    , validatePotentialDomains
    , constructRuleTree
    , applyRules
    )
where

import BasePrelude

import Control.Applicative (liftA2)
import Control.Monad.Extra (partitionM, concatMapM, ifM)
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Data.List.NonEmpty as NE
import Named ((:!), arg)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, getPermissions, pathIsSymbolicLink, readable)
import System.FilePath ((</>), makeRelative, normalise, splitDirectories)
import qualified Data.Yaml as Yaml
import Data.Text (Text)
import qualified Data.Text as T

import Fencer.Types


data LoadRulesError
  = LoadRulesParseError FilePath Yaml.ParseException
  | LoadRulesIOError IOException
  | LoadRulesDuplicateDomain DomainId
  | LoadRulesDuplicateRule DomainId (RuleKey, Maybe RuleValue)
  deriving stock (Show)

-- | Pretty-print a 'LoadRulesError'.
showError :: LoadRulesError -> String
showError (LoadRulesParseError file yamlEx) =
  show file ++ ", " ++ (Yaml.prettyPrintParseException yamlEx)
showError (LoadRulesIOError ex) = "IO error: " ++ displayException ex
showError (LoadRulesDuplicateDomain d) =
  "duplicate domain " ++ (show . unDomainId $ d) ++ " in config file"
showError (LoadRulesDuplicateRule dom (key, val)) =
  "duplicate descriptor composite key " ++
  show (unDomainId dom) ++ "." ++ show (unRuleKey key) ++
  case val of
    Nothing -> ""
    Just v -> "." ++ show (unRuleValue v)

-- | Pretty-print a list of 'LoadRulesError's.
prettyPrintErrors :: [LoadRulesError] -> String
prettyPrintErrors = intercalate ", " . fmap showError

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
    -> IO (Either (NonEmpty LoadRulesError) [DomainDefinition])
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
    validatePotentialDomains <$> mapM loadFile filteredFiles
  where
    loadFile :: FilePath -> IO (Either LoadRulesError (Maybe DomainDefinition))
    loadFile file =
      ifM (readable <$> getPermissions file)
        (catch
          (convertParseType file <$> Yaml.decodeFileEither @DomainDefinition file)
          (pure . Left . LoadRulesIOError)
        )
        (pure $ Right Nothing)

    -- | Convert to the needed sum type.
    convertParseType
      :: FilePath
      -> Either Yaml.ParseException DomainDefinition
         ----------------------------------------------
      -> Either LoadRulesError (Maybe DomainDefinition)
    convertParseType _    (Right def) = Right $ Just def
    convertParseType file (Left err)  = Left $ LoadRulesParseError file err

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

-- | Perform validation checks to make sure the behavior matches that
-- of @lyft/ratelimit@.
validatePotentialDomains
  :: [Either LoadRulesError (Maybe DomainDefinition)]
  -> Either (NonEmpty LoadRulesError) [DomainDefinition]
validatePotentialDomains res = case partitionEithers res of
  (errs@(_:_), _       ) -> Left $ NE.fromList errs
  ([]        , []      ) -> Right []
  ([]        , mDomains) -> do
    -- check if there are any duplicate domains
    let domains = catMaybes mDomains
    let dupDomains =
          filter (\ds -> length @[] ds > 1) $
          groupWith domainDefinitionId domains
    unless (null dupDomains) $
      Left $ NE.fromList
        [LoadRulesDuplicateDomain (domainDefinitionId dupDomain)
          | dupDomain <- map head dupDomains]
    -- check if there are any duplicate rules
    traverse_ (\dom -> dupRuleCheck (domainDefinitionId dom, dom)) domains
    pure domains
 where
  dupRuleCheck
    :: HasDescriptors a
    => (DomainId, a)
    -> Either (NonEmpty LoadRulesError) ()
  dupRuleCheck (domId, d) = do
    let dupDescs =
          filter (\ds -> length @[] ds > 1) $
          groupWith (\x -> (descriptorDefinitionKey x, descriptorDefinitionValue x)) $
          descriptorsOf d
    unless (null dupDescs) $
      Left $ NE.fromList
        [LoadRulesDuplicateRule
           domId
           (descriptorDefinitionKey dupRule, descriptorDefinitionValue dupRule)
           | dupRule <- map head dupDescs]
    traverse_ (curry dupRuleCheck domId) $ descriptorsOf d

-- | Convert a domain to a 'RuleTree' together with the domain ID.
constructRuleTree :: DomainDefinition -> (DomainId, RuleTree)
constructRuleTree domain =
    (domainId, go [] (domainDefinitionDescriptors domain))
  where
    domainId :: DomainId
    domainId = domainDefinitionId domain

    go :: [Text]  -- Current path in the rule tree
       -> [DescriptorDefinition]
       -> RuleTree
    go path descriptors = HM.fromList $ map (makeBranch path) descriptors

    makeBranch
        :: [Text]
        -> DescriptorDefinition
        -> ((RuleKey, Maybe RuleValue), RuleBranch)
    makeBranch path desc =
        let key = (descriptorDefinitionKey desc, descriptorDefinitionValue desc)
            textKey = case key of
                (k, Nothing) -> unRuleKey k
                (k, Just v) -> unRuleKey k <> "_" <> unRuleValue v
            branch = case desc of
                DescriptorDefinitionLeafNode _ _ limit ->
                    RuleBranchLeaf $ RuleLeaf
                      { ruleLeafStatsKey = StatsKey $
                          T.intercalate "." $
                          unDomainId domainId : path ++ [textKey]
                      , ruleLeafLimit = limit }
                DescriptorDefinitionInnerNode _ _ descs ->
                    RuleBranchTree (go (path ++ [textKey]) descs)
        in (key, branch)

-- | In a tree of rules, find the leaf that should be applied to a specific
-- descriptor.
applyRules :: [(RuleKey, RuleValue)] -> RuleTree -> Maybe RuleLeaf
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
    case (null @[] rest, branch) of
        (True,  RuleBranchLeaf leaf)  -> Just leaf
        (True,  RuleBranchTree _)     -> Nothing
        (False, RuleBranchLeaf _)     -> Nothing
        (False, RuleBranchTree tree') -> applyRules rest tree'
