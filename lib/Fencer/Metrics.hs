{-# LANGUAGE OverloadedStrings #-}

-- | A module for tracking several metrics and interacting with a
-- statsd server.
module Fencer.Metrics
  ( fencerNamespace
  , limitToPath
  ) where

import           BasePrelude

import           Data.Text (unpack)

import           Fencer.Types
                 ( DomainId(..)
                 , RuleKey(..)
                 , RuleValue(..)
                 )


-- | The namespace provided by Fencer for metrics. It is used in
-- logging and with statsd.
fencerNamespace :: String
fencerNamespace = "fencer.service.rate_limit"

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
  ([fencerNamespace, unpack domain] ++) .
  fmap showKeyValue

