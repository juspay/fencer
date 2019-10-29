-- | A module for adjusting log level values to the title case. This
-- is needed to make sure tinylog works with value cases used by
-- lfyt/ratelimit.
module Fencer.Logger (adjustCase) where


import BasePrelude

import Data.Text.Titlecase (titlecase)


-- | Adjust the value case of the LOG_LEVEL environment variable so
-- that it works with tinylog.
adjustCase :: IO ()
adjustCase = do
  mLvl <- lookupEnv logLevelVar
  case mLvl of
    Nothing  -> pure ()
    Just lvl -> setEnv logLevelVar $ titlecase lvl
 where
  logLevelVar = "LOG_LEVEL"
