module Ratelimit.Types
    (
    -- * Rate limiting rules
      DomainId(..)
    , Domain(..)
    , Descriptor(..)
    , TimeUnit(..)
    , RateLimit(..)

    -- * Request
    , Request
    )
where

import Data.Text (Text)

----------------------------------------------------------------------------
-- Rules
----------------------------------------------------------------------------

newtype DomainId = DomainId Text

data Domain = Domain
    { domainId :: DomainId
    , domainDescriptors :: [Descriptor]
    }

newtype RuleKey = RuleKey Text

newtype RuleValue = RuleValue Text

data Descriptor = Descriptor
    { descriptorKey :: RuleKey
    , descriptorValue :: Maybe RuleValue
    , descriptorRateLimit :: Maybe RateLimit
    , descriptorDescriptors :: Maybe [Descriptor]
    }

data TimeUnit = Second | Minute | Hour | Day

data RateLimit = RateLimit
    { rateLimitUnit :: TimeUnit
    , rateLimitRequestsPerUnit :: Word
    }

----------------------------------------------------------------------------
-- Request
----------------------------------------------------------------------------

data Request = Request
    { requestDomain :: DomainId
    , requestDescriptor :: [(RuleKey, RuleValue)]
    }
