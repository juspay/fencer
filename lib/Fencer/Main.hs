{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

-- | The main module of Fencer, included in the library to make it easier to
-- load it in REPL. @src/Main.hs@ simply calls 'main' from this module.
module Fencer.Main
    ( main
    )
where

import BasePrelude
import Control.Concurrent.STM (atomically)
import Named
import System.Directory (listDirectory, doesFileExist, makeAbsolute)
import System.FilePath
import qualified StmContainers.Map as StmMap
import qualified Data.Yaml as Yaml
import qualified System.FSNotify as FSNotify

import Fencer.Types
import Fencer.AppState
import Fencer.Server
import Fencer.Match
import Fencer.Settings

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

-- | Load YAML rules from @config/@, create in-memory state and run the gRPC
-- server serving ratelimit requests.
main :: IO ()
main = do
    appState <- initAppState
    settings <- getSettingsFromEnvironment
    reloadRules settings appState
    watchRoot
        (#root (settingsRoot settings))
        (#onChange (reloadRules settings appState))
    runServer appState

----------------------------------------------------------------------------
-- Load rules
----------------------------------------------------------------------------

-- | Gather rate limiting rules (*.yml, *.yaml) from a directory.
-- Subdirectories are not included.
--
-- Throws an exception for unparseable or unreadable files.
parseRules
  :: "directory" :! FilePath
  -> "ignoreDotFiles" :! Bool  -- ^ Ignore hidden files (starting with a dot)
  -> IO [DomainDefinition]
parseRules (arg #directory -> directory) (arg #ignoreDotFiles -> ignoreDotFiles) = do
    files <- filterM doesFileExist . map (directory </>) =<< listDirectory directory
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

-- | Clear the rule storage and reload rules from the @config/@
-- subdirectory.
reloadRules :: Settings -> AppState -> IO ()
reloadRules settings appState = do
    let configDir = settingsRoot settings </> settingsSubdirectory settings </> "config"
    putStrLn ("Loading rules from " ++ configDir)
    rules <- parseRules
        (#directory configDir)
        (#ignoreDotFiles (settingsIgnoreDotFiles settings))
    atomically $ do
        StmMap.reset (appStateRules appState)
        forM_ rules $ \rule -> do
            let domain = domainDefinitionId rule
                tree = makeRuleTree (domainDefinitionDescriptors rule)
            StmMap.insert tree domain (appStateRules appState)

----------------------------------------------------------------------------
-- Directory watching
----------------------------------------------------------------------------

-- | Fork a thread that watches the settings root (which should be a
-- symlink) and executes an action when it's replaced with another symlink.
watchRoot
    :: "root" :! FilePath
    -> "onChange" :! IO ()
    -> IO ()
watchRoot (arg #root -> root) (arg #onChange -> onChange) =
    void $ forkOS $ FSNotify.withManager $ \manager -> do
        -- We watch the directory *containing* the settings root, and we
        -- watch for 'Added' or 'Modified' events. Note that fsnotify
        -- doesn't allow watching a file, only a directory.
        absoluteRoot <- makeAbsolute root
        let directory = takeDirectory absoluteRoot
        let predicate = \case
                FSNotify.Added path _ _ ->
                    path == absoluteRoot
                FSNotify.Modified path _ _ ->
                    path == absoluteRoot
                _ ->
                    False
        _ <- FSNotify.watchDir manager directory predicate $ \_ -> onChange
        forever $ threadDelay 1000000
