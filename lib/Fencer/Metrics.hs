{-# LANGUAGE OverloadedStrings #-}

-- | A module for tracking several metrics and interacting with a
-- statsd server.
module Fencer.Metrics
  ( limitToPath
  , threeMetrics
  , fencerNamespace
  , nearLimitLabel
  , overLimitLabel
  , totalHitsLabel
  ) where

import           BasePrelude

import           Data.Text (Text, pack, unpack)

import           Fencer.Counter (CounterStatus(..))
import           Fencer.Settings (Settings, settingsNearLimitRatio)
import           Fencer.Types
                 ( DomainId(..)
                 , HitCount(..)
                 , OverLimitCount(..)
                 , RateLimit(..)
                 , RuleKey(..)
                 , RuleValue(..)
                 )


-- | The namespace provided by Fencer for metrics. It is used in
-- logging. The statsd prefix is provided in Fencer.Main.main during
-- forking of a server thread.
fencerNamespace :: String
fencerNamespace = "stats.fencer.service.rate_limit"

-- | A string constant "near_limit".
nearLimitLabel :: String
nearLimitLabel = "near_limit"

-- | A string constant "over_limit".
overLimitLabel :: String
overLimitLabel = "over_limit"

-- | A string constant "total_hits".
totalHitsLabel :: String
totalHitsLabel = "total_hits"

-- | Convert a rule key and value to a partial path.
showKeyValue :: (RuleKey, RuleValue) -> String
showKeyValue (RuleKey k, RuleValue v) = unpack k ++ case v of
  "" -> ""
  _  -> "_" ++ unpack v

-- | Convert a domain id and a list of pairs of 'RuleKey's and
-- 'RuleValue's to a namespace path.
limitToPath
  :: DomainId
  -> [(RuleKey, RuleValue)]
  -> String
limitToPath (DomainId domain) =
  intercalate "." .
  ([unpack domain] ++) .
  fmap showKeyValue

-- | Compute the near limit statistic based on a rate limit and
-- a counter status.
statNearLimit
  :: Settings
  -> RateLimit
  -> CounterStatus
  -> Word
statNearLimit settings rateLimit status =
  let
    limit = rateLimitRequestsPerUnit rateLimit
    nearRatio = settingsNearLimitRatio settings
    threshold = round $
      fromIntegral limit * nearRatio
    remaining = counterRemainingLimit status
  in if (counterHitsOverLimit status /= 0) ||
          (remaining + threshold > limit)
       then 0
       else limit - threshold - remaining

-- | For a given domain and a descriptor, compute a list of metrics
-- mapping. The mapping is from a metric name to a function that for a
-- given sample of a metric group returns the metric's value.
threeMetrics
  :: Settings
  -> DomainId
  -> [(RuleKey, RuleValue)]
  -> [(Text, (RateLimit, OverLimitCount, HitCount) -> Word)]
threeMetrics settings domain descriptor =
  [ ( pack $ prefix descriptor ++ "." ++ nearLimitLabel
    -- , uncurry (statNearLimit settings) .
    , const 100 .
      \(l, s, _) -> (l, s) )
  , ( pack $ prefix descriptor ++ "." ++ overLimitLabel
    , unOverLimitCount . \(_, s, _) -> s )
  , ( pack $ prefix descriptor ++ "." ++ totalHitsLabel
    , unHitCount . \(_, _, c) -> c )
  ]
 where
  prefix :: [(RuleKey, RuleValue)] -> String
  prefix = limitToPath domain
