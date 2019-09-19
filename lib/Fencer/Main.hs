{-# LANGUAGE NumericUnderscores #-}
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
import Named ((:!), arg)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName)
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
    -- Create in-memory state
    appState <- initAppState
    -- Read environment variables
    settings <- getSettingsFromEnvironment
    -- Load rate limiting rules for the first time
    reloadRules logger settings appState
    -- Create a thread watching the config directory for changes
    watchSymlink
        (#symlink (settingsRoot settings))
        (#onChange (reloadRules logger settings appState))
    -- Create a thread for updating current time every 1ms and removing
    -- expired counters
    void $ forkIO $ forever $ do
        threadDelay 1_000
        (before, now) <- updateCurrentTime appState
        -- Remove expired counters in a new thread. If it takes too long,
        -- ticks would still happen as often as they usually do, and
        -- 'updateCurrentTime' would still be done every 1 ms.
        when (now > before) $ void $ forkIO $
            -- Note: we say "pred now" so that we would never run
            -- 'deleteCountersWithExpiry' twice for the same timestamp.
            --
            -- TODO: clarify the counter removal logic?
            mapM_ (deleteCountersWithExpiry appState) [before .. pred now]
    -- Start the gRPC server
    runServer logger appState

----------------------------------------------------------------------------
-- Load rules
----------------------------------------------------------------------------

-- | Clear the rule storage and reload rules from the @config/@
-- subdirectory.
reloadRules :: Logger -> Settings -> AppState -> IO ()
reloadRules logger settings appState = do
    let configDir =
            settingsRoot settings </>
            settingsSubdirectory settings </>
            "config"
    Logger.info logger $
        Logger.msg ("Loading rules from " ++ configDir)

    -- Read and parse the rules
    ruleDefinitions :: [DomainDefinition] <-
        loadRulesFromDirectory
            (#directory configDir)
            (#ignoreDotFiles (settingsIgnoreDotFiles settings))
    Logger.info logger $
        Logger.msg ("Parsed rules for domains: " ++
                    show (map (unDomainId . domainDefinitionId) ruleDefinitions))

    -- Recreate 'appStateRules'
    atomically $
        setRules appState
            [ ( domainDefinitionId rule
              , definitionsToRuleTree (domainDefinitionDescriptors rule))
            | rule <- ruleDefinitions
            ]
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
