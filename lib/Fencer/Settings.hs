{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | Fencer start-time config.
module Fencer.Settings
    ( Settings(..)
    , getSettingsFromEnvironment
    , defaultGRPCPort
    )
where

import BasePrelude

import Text.Read (readMaybe)

import Fencer.Types (Port(..))

-- | The default port for a gRPC server
defaultGRPCPort :: Port
defaultGRPCPort = Port 50051

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
    pure Settings{..}

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
