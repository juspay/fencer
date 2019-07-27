{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratelimit.Types
    (
    -- * Time units
      TimeUnit(..)
    , timeUnitToSeconds

    -- * Rate limiting rules
    , DomainId(..)
    , Domain(..)
    , RuleKey(..)
    , RuleValue(..)
    , Descriptor(..)
    , RateLimit(..)

    -- * Request
    , Request
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Data.Text (Text)

----------------------------------------------------------------------------
-- Time units
----------------------------------------------------------------------------

data TimeUnit = Second | Minute | Hour | Day
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

-- | Return the duration of a 'TimeUnit'.
timeUnitToSeconds :: TimeUnit -> Int64
timeUnitToSeconds = \case
    Second -> 1
    Minute -> 60
    Hour -> 3600
    Day -> 86400

----------------------------------------------------------------------------
-- Rate limiting rules
----------------------------------------------------------------------------

newtype DomainId = DomainId Text
    deriving stock (Eq)
    deriving newtype (Hashable)

data Domain = Domain
    { domainId :: !DomainId
    , domainDescriptors :: ![Descriptor]
    }

newtype RuleKey = RuleKey Text
    deriving stock (Eq)
    deriving newtype (Hashable)

newtype RuleValue = RuleValue Text
    deriving stock (Eq)
    deriving newtype (Hashable)

data Descriptor = Descriptor
    { descriptorKey :: !RuleKey
    , descriptorValue :: !(Maybe RuleValue)
    , descriptorRateLimit :: !(Maybe RateLimit)
    , descriptorDescriptors :: !(Maybe [Descriptor])
    }

data RateLimit = RateLimit
    { rateLimitUnit :: !TimeUnit
    , rateLimitRequestsPerUnit :: !Word
    }

----------------------------------------------------------------------------
-- Request
----------------------------------------------------------------------------

data Request = Request
    { requestDomain :: !DomainId
    , requestDescriptor :: ![(RuleKey, RuleValue)]
    }
