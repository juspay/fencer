{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | Fencer start-time config.
module Fencer.Settings
    ( Settings(..)
    , getSettingsFromEnvironment
    , defaultGRPCPort
    , getLogLevel
    , newLogger
    )
where

import BasePrelude

import           Data.Char (toLower)
import qualified System.Logger as Logger
import           Text.Read (readMaybe)

import           Fencer.Types (Port(..))

-- | The default port for a gRPC server
defaultGRPCPort :: Port
defaultGRPCPort = Port 8081

-- | Fencer settings.
data Settings = Settings
    { -- | @RUNTIME_ROOT@: symlink to a directory containing
      -- 'settingsSubdirectory'. The default value is
      -- @\/srv\/runtime_data\/current@.
      settingsRoot :: FilePath
      -- | @RUNTIME_SUBDIRECTORY@: directory with settings.
    , settingsSubdirectory :: FilePath
      -- | @RUNTIME_IGNOREDOTFILES@: whether to ignore files with names
      -- starting with a dot (hidden files on Linux and macOS). The default
      -- value is @false@.
    , settingsIgnoreDotFiles :: Bool
      -- | @GRPC_PORT@: gRPC port to run the server on. The default
      -- value is 'defaultGRPCPort'.
    , settingsGRPCPort :: Port
      -- | @LOG_LEVEL@: the logging level. This is a value translated
      -- from Go's logrus logging library to a value accepted by
      -- Haskell's tinylog library. The default value is Logger.Debug.
    , settingsLogLevel :: Logger.Level
      -- | @USE_STATSD@: whether to create statistics for rate
      -- limits. The default value is @false@.
    , settingsUseStatsd :: Bool
      -- | A ratio informing the number of hits to the rate limit at
      -- which it is near the rate limit. For now the user has no way
      -- of configuring it and it is always set to 0.8.
    , settingsNearLimitRatio :: Double
    }
    deriving (Show)

-- | Get 'Settings' from environment variables. See 'Settings' documentation
-- for variable names and default values.
getSettingsFromEnvironment :: IO Settings
getSettingsFromEnvironment = do
    settingsRoot <- fromMaybe "/srv/runtime_data/current" <$> lookupEnv "RUNTIME_ROOT"
    settingsSubdirectory <- getEnv "RUNTIME_SUBDIRECTORY"
    settingsIgnoreDotFiles <- lookupEnv "RUNTIME_IGNOREDOTFILES" >>= \case
        Nothing -> pure False
        Just s -> case parseBool s of
            Just b -> pure b
            Nothing -> error ("Could not parse RUNTIME_IGNOREDOTFILES: " ++ show s)
    settingsGRPCPort <- lookupEnv "GRPC_PORT" >>= \case
        Nothing -> pure defaultGRPCPort
        Just s  -> case readMaybe @Word s of
            Nothing -> error ("Could not parse GRPC_PORT: " ++ show s)
            Just p  -> pure $ Port p
    settingsLogLevel <- getLogLevel
    settingsUseStatsd <- lookupEnv "USE_STATSD" >>= \case
      Nothing -> pure False
      Just s  -> case parseBool s of
        Just b  -> pure b
        Nothing -> error ("Could not parse USE_STATSD: " ++ show s)
    let settingsNearLimitRatio = 0.8
    pure Settings{..}

-- | Get 'Logger.Level' from the environment variable LOG_LEVEL and
-- map it to tinylog's 'System.Logger.Level'.
getLogLevel :: IO Logger.Level
getLogLevel = parseLogLevel <$> lookupEnv "LOG_LEVEL"

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Parse a boolean the same way Go does:
-- <https://golang.org/pkg/strconv/#ParseBool>.
parseBool :: String -> Maybe Bool
parseBool s
    | s `elem` ["1", "t", "T", "TRUE", "true", "True"] = Just True
    | s `elem` ["0", "f", "F", "FALSE", "false", "False"] = Just False
    | otherwise = Nothing

-- | Parse the string value of the LOG_LEVEL environment variable and
-- map it to a 'System.Logger.Level'. Use the default Debug level if
-- the environment variable was not set.
parseLogLevel :: Maybe String -> Logger.Level
parseLogLevel Nothing  = Logger.Debug
parseLogLevel (Just s) = case readMaybe @Logger.Level s of
  Just l  -> l
  Nothing -> case toLower <$> s of
    "panic"   -> Logger.Fatal -- tinylog does not have Panic
    "fatal"   -> Logger.Fatal
    "error"   -> Logger.Error
    "warn"    -> Logger.Warn
    "warning" -> Logger.Warn
    "info"    -> Logger.Info
    "debug"   -> Logger.Debug
    "trace"   -> Logger.Trace
    _         -> error ("Could not parse LOG_LEVEL: " ++ s)

-- | Creates a new loger from default settings, where the output and
-- logging level are overriden with values provided as function
-- arguments. Reading from the environment is disabled in creating the
-- logger.
newLogger :: MonadIO m => Logger.Output -> Logger.Level -> m Logger.Logger
newLogger output level = Logger.new $
  Logger.setOutput output $
  Logger.setLogLevel level $
  Logger.setReadEnvironment False Logger.defSettings
