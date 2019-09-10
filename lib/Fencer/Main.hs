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
import System.Directory (listDirectory, doesFileExist)
import System.FilePath
import qualified StmContainers.Map as StmMap
import qualified Data.Yaml as Yaml
import qualified System.Logger as Logger
import System.Logger (Logger)

import Fencer.Types
import Fencer.AppState
import Fencer.Server
import Fencer.Rules
import Fencer.Watch
import Fencer.Settings

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

-- | Load YAML rules from @config/@, create in-memory state and run the gRPC
-- server serving ratelimit requests.
main :: IO ()
main = do
    -- Initialize logging
    logger <- Logger.new $
        Logger.setOutput Logger.StdErr $
        Logger.defSettings
    -- Create in-memory state and read environment variables
    appState <- initAppState
    -- Load rate limiting rules for the first time
    reloadRules logger appState
    -- Create a thread watching the config directory for changes
    watchSymlink
        (#symlink (settingsRoot (appStateSettings appState)))
        (#onChange (reloadRules logger appState))
    -- Start the gRPC server
    runServer logger appState

----------------------------------------------------------------------------
-- Load rules
----------------------------------------------------------------------------

-- | Clear the rule storage and reload rules from the @config/@
-- subdirectory.
reloadRules :: Logger -> AppState -> IO ()
reloadRules logger appState = do
    let configDir =
            settingsRoot (appStateSettings appState) </>
            settingsSubdirectory (appStateSettings appState) </>
            "config"
    Logger.info logger $
        Logger.msg ("Loading rules from " ++ configDir)

    -- Read and parse the rules
    rules :: [DomainDefinition] <-
        loadRulesFromDirectory
            (#directory configDir)
            (#ignoreDotFiles (settingsIgnoreDotFiles (appStateSettings appState)))
    Logger.info logger $
        Logger.msg ("Parsed rules for domains: " ++
                    show (map (unDomainId . domainDefinitionId) rules))

    -- Recreate 'appStateRules'
    atomically $ do
        StmMap.reset (appStateRules appState)
        forM_ rules $ \rule -> do
            let domain = domainDefinitionId rule
                tree = definitionsToRuleTree (domainDefinitionDescriptors rule)
            StmMap.insert tree domain (appStateRules appState)
    Logger.info logger $
        Logger.msg (Logger.val "Applied new rules")

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
