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

import qualified Data.List.NonEmpty as NE
import System.FilePath ((</>))
import qualified System.Logger as Logger
import System.Logger (Logger)
import qualified System.Metrics as Ekg
import qualified System.Remote.Monitoring.Statsd as Statsd

import Fencer.Types
import Fencer.Logic
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
    -- Read environment variables
    settings <- getSettingsFromEnvironment
    -- Initialize logging
    logger <- newLogger Logger.StdErr (settingsLogLevel settings)
    -- Create in-memory state
    appState <- initAppState
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
    -- Set up monitoring
    ekgStore <- Ekg.newStore
    when (settingsUseStatsd settings) $ do
      let statsdOptions = Statsd.StatsdOptions
            { Statsd.host = "127.0.0.1"
            , Statsd.port = 8125
            , Statsd.flushInterval = 5000 -- 5s
            , Statsd.debug = False
            , Statsd.prefix = "fencer.service.rate_limit"
            , Statsd.suffix = "" }
      void $ Statsd.forkStatsd statsdOptions ekgStore
    -- Start the gRPC server
    runServer
      (settingsGRPCPort settings)
      logger
      (#useStatsd (settingsUseStatsd settings))
      appState

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
    loadRulesFromDirectory
        (#rootDirectory $ settingsRoot settings)
        (#subDirectory $ settingsSubdirectory settings </> "config")
        (#ignoreDotFiles (settingsIgnoreDotFiles settings))
    >>= \case
    Left fs ->
        Logger.err logger $
            Logger.msg ("error loading new configuration from runtime: " ++
                        prettyPrintErrors (NE.toList fs))
    Right ruleDefinitions -> do
        Logger.info logger $
            Logger.msg ("Parsed rules for domains: " ++
                show (map (unDomainId . domainDefinitionId) ruleDefinitions))
        -- Recreate 'appStateRules'. There is no need to remove old rate
        -- limiting rules. See the documentation of 'setRules' for
        -- details on what happens with counters during rule reloading.
        setRules appState (map constructRuleTree ruleDefinitions)
        Logger.info logger $
            Logger.msg (Logger.val "Applied new rules")
