{-# LANGUAGE RecordWildCards #-}

-- | Fencer start-time config.
module Fencer.Settings
    ( Settings(..)
    , getSettingsFromEnvironment
    )
where

import BasePrelude

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
