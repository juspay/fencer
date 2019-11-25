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

import qualified Data.Text.IO as TIO
import           Named ((:!), arg)
import qualified System.Directory as Dir
import           System.FilePath (FilePath, takeDirectory, (</>))
import qualified System.IO.Temp as Temp
import           Test.Tasty.HUnit (assertBool, assertEqual, Assertion)

import           Fencer.Rules (LoadRulesError(..), loadRulesFromDirectory)
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
  -> IO (Either [LoadRulesError] [DomainDefinition])
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
  -> "result" :! Either [LoadRulesError] [DomainDefinition]
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
      f@(Left _) ->
        -- Paths to temporary files vary and there is not much point
        -- in writing down exact expected exception messages so the
        -- only assertion made is that the number of exceptions is the
        -- same.
        assertEqual
          "unexpected failure"
          (length . toErrorList $ result)
          (length . toErrorList $ f)
      Right definitions -> assertBool "unexpected definitions"
        (((==) `on` show)
        (sortOn domainDefinitionId <$> result)
        (Right $ sortOn domainDefinitionId definitions))

