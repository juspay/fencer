{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}

-- | Module with helper functions used in rules and other testing.
module Fencer.Rules.Test.Helpers
  ( toErrorList
  , writeContentsToFile
  , writeAndLoadRules
  , expectLoadRules
  )
where

import           BasePrelude

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as TIO
import           Named ((:!), arg)
import qualified System.Directory as Dir
import           System.FilePath (FilePath, takeDirectory, takeFileName, (</>))
import qualified System.IO.Temp as Temp
import           Test.Tasty.HUnit (assertBool, assertFailure, Assertion)

import           Fencer.Rules (LoadRulesError(..), loadRulesFromDirectory, prettyPrintErrors, showError)
import           Fencer.Rules.Test.Types (RuleFile(..))
import           Fencer.Types (DomainDefinition(..))


-- | Get a list of values on the Left or an empty list if it is a
-- Right value.
toErrorList :: Either [a] [b] -> [a]
toErrorList (Right _) = []
toErrorList (Left xs) = xs

-- | Write contents to a path in the given root and modify file
-- permissions.
writeContentsToFile
  :: "root" :! FilePath
  -> "file" :! RuleFile
  -> IO ()
writeContentsToFile
  (arg #root -> root)
  (arg #file -> file) = do

  let
    dir = takeDirectory (ruleFilePath file)
    fullPath = root </> (ruleFilePath file)
  Dir.createDirectoryIfMissing True (root </> dir)
  TIO.writeFile fullPath (ruleFileContents file)
  perms <- Dir.getPermissions fullPath
  Dir.setPermissions fullPath (ruleFileModifyPermissions file perms)

-- | Write the content of files at the given root and load the files.
writeAndLoadRules
  :: "ignoreDotFiles" :! Bool
  -> "root" :! FilePath
  -> "files" :! [RuleFile]
  -> IO (Either (NonEmpty LoadRulesError) [DomainDefinition])
writeAndLoadRules
  (arg #ignoreDotFiles -> ignoreDotFiles)
  (arg #root -> root)
  (arg #files -> files) = do

  forM_ files $ \file -> writeContentsToFile
    (#root root)
    (#file file)
  loadRulesFromDirectory
    (#rootDirectory root)
    (#subDirectory ".")
    (#ignoreDotFiles ignoreDotFiles)

-- | Create given directory structure and check that
-- 'loadRulesFromDirectory' produces expected result such that file
-- permissions are configurable.
expectLoadRules
  :: "ignoreDotFiles" :! Bool
  -> "files" :! [RuleFile]
  -> "result" :! Either (NonEmpty LoadRulesError) [DomainDefinition]
  -> Assertion
expectLoadRules
  (arg #ignoreDotFiles -> ignoreDotFiles)
  (arg #files -> files)
  (arg #result -> result) =
  Temp.withSystemTempDirectory "fencer-config" $ \tempDir ->
    writeAndLoadRules
      (#ignoreDotFiles ignoreDotFiles)
      (#root tempDir)
      (#files files)
      >>= \case
      Left errs ->
        case result of
          Right _ ->
            assertFailure "Expected failures, got domain definitions!"
          Left expectedErrs ->
            assertBool ("Exceptions differ! Expected: " ++
                        (prettyPrintErrors $ NE.toList expectedErrs) ++ "\nGot: " ++
                        (prettyPrintErrors $ NE.toList errs))
              (((==) `on` (fmap showError))
               (NE.sortBy (compare `on` showError) (trimPath <$> expectedErrs))
               (NE.sortBy (compare `on` showError) (trimPath <$> errs)))
      Right definitions -> assertBool "unexpected definitions"
        (((==) `on` show)
        (sortOn domainDefinitionId <$> result)
        (Right $ sortOn domainDefinitionId definitions))
 where
  trimPath :: LoadRulesError -> LoadRulesError
  trimPath (LoadRulesParseError p ex) = LoadRulesParseError (takeFileName p) ex
  trimPath e                          = e
