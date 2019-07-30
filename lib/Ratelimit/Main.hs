{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}

module Ratelimit.Main
    ( main
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Control.Concurrent.STM (atomically, orElse)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Named
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Focus as Focus
import qualified StmContainers.Map as StmMap
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite

import Ratelimit.Types
import Ratelimit.Counter
import Ratelimit.Time
import qualified Ratelimit.Proto as Proto

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

main :: IO ()
main = do
    storage <- newStorage
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit = shouldRateLimit storage }
    let options = Grpc.defaultServiceOptions
    Proto.rateLimitServiceServer handlers options

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

-- | In-memory storage for the state of the program.
--
-- TODO: garbage collect counters every once in a while.
data Storage = Storage
    { rules :: !(StmMap.Map CounterKey RateLimit)
    , counters :: !(StmMap.Map CounterKey Counter)
    }

-- | Create an empty 'Storage'.
newStorage :: IO Storage
newStorage = Storage <$> StmMap.newIO <*> StmMap.newIO

----------------------------------------------------------------------------
-- "Should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Storage
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit storage (Grpc.ServerNormalRequest _metadata request) = do
    let domain = DomainId (TL.toStrict (Proto.rateLimitRequestDomain request))
    let descriptors =
            map rateLimitDescriptorFromProto $
            toList (Proto.rateLimitRequestDescriptors request)
    let keys = map
            (\descriptor -> CounterKey
                 { counterKeyDomain = domain
                 , counterKeyDescriptor = descriptor })
            descriptors
    -- Note: 'rateLimitRequestHitsAddend' could be 0 (default value) if not
    -- specified by the caller in the request.
    let hits :: Word
        hits = max 1 (fromIntegral (Proto.rateLimitRequestHitsAddend request))
    now <- getTimestamp
    -- TODO: this might retry way too often if we touch too many keys. Need
    -- to figure out whether it's safe to do each operation independently.
    (codes, statuses) <-
        fmap (unzip . map (maybe ruleNotFoundResponse counterStatusToProto)) $
        atomically $
        mapM (shouldRateLimitDescriptor storage (#now now) (#hits hits)) keys
    let answer = Proto.RateLimitResponse
            { Proto.rateLimitResponseOverallCode =
                  ProtoSuite.Enumerated $
                  Right $
                  if Proto.RateLimitResponse_CodeOVER_LIMIT `elem` codes
                      then Proto.RateLimitResponse_CodeOVER_LIMIT
                      else Proto.RateLimitResponse_CodeOK
            , Proto.rateLimitResponseStatuses = V.fromList statuses
            , Proto.rateLimitResponseHeaders = mempty
            }
    pure $ Grpc.ServerNormalResponse
        -- answer
        answer
        -- metadata
        mempty
        -- status
        Grpc.StatusOk
        -- status details
        ""

-- | Handle a single descriptor in a 'shouldRateLimit' request.
--
-- Returns the current limit and encoded response
shouldRateLimitDescriptor
    :: Storage
    -> "now" :! Timestamp
    -> "hits" :! Word
    -> CounterKey
    -> STM (Maybe (RateLimit, CounterStatus))
shouldRateLimitDescriptor storage (arg #now -> now) (arg #hits -> hits) key =
    StmMap.lookup key (rules storage) >>= \case
        Nothing -> pure Nothing
        Just limit -> Just <$> StmMap.focus (update limit) key (counters storage)
  where
    -- Update the counter corresponding to 'key', or create a new counter if
    -- it does not exist.
    update :: RateLimit -> Focus.Focus Counter STM (RateLimit, CounterStatus)
    update limit = Focus.lookup >>= \case
        Nothing -> do
            let (counter, status) =
                    updateCounter (#now now) (#hits hits) (#limit limit) $
                    newCounter (#now now) (#limit limit)
            Focus.insert counter
            pure (limit, status)
        Just counter -> do
            let (newCounter, status) =
                    updateCounter (#now now) (#hits hits) (#limit limit) counter
            Focus.insert newCounter
            pure (limit, status)

----------------------------------------------------------------------------
-- Working with protobuf structures
----------------------------------------------------------------------------

-- | Convert a 'RateLimit' to the protobuf representation.
rateLimitToProto :: RateLimit -> Proto.RateLimit
rateLimitToProto limit = Proto.RateLimit
    { Proto.rateLimitRequestsPerUnit =
          fromIntegral (rateLimitRequestsPerUnit limit)
    , Proto.rateLimitUnit =
          ProtoSuite.Enumerated . Right $
          case rateLimitUnit limit of
              Second -> Proto.RateLimit_UnitSECOND
              Minute -> Proto.RateLimit_UnitMINUTE
              Hour -> Proto.RateLimit_UnitHOUR
              Day -> Proto.RateLimit_UnitDAY
    }

-- | Convert a 'CounterStatus' with the current counter rate limit to the
-- protobuf representation.
counterStatusToProto
    :: (RateLimit, CounterStatus)
    -> (Proto.RateLimitResponse_Code, Proto.RateLimitResponse_DescriptorStatus)
counterStatusToProto (limit, status) =
    ( code
    , Proto.RateLimitResponse_DescriptorStatus
          { Proto.rateLimitResponse_DescriptorStatusCode =
                ProtoSuite.Enumerated (Right code)
          , Proto.rateLimitResponse_DescriptorStatusCurrentLimit =
                Just (rateLimitToProto limit)
          , Proto.rateLimitResponse_DescriptorStatusLimitRemaining =
                fromIntegral (counterRemainingLimit status)
          }
    )
  where
    code = if counterHitsOverLimit status > 0
        then Proto.RateLimitResponse_CodeOVER_LIMIT
        else Proto.RateLimitResponse_CodeOK

-- | The response that we should return when the rate limit rule was not found.
ruleNotFoundResponse
    :: (Proto.RateLimitResponse_Code, Proto.RateLimitResponse_DescriptorStatus)
ruleNotFoundResponse =
    ( Proto.RateLimitResponse_CodeOK
    , Proto.RateLimitResponse_DescriptorStatus
          { Proto.rateLimitResponse_DescriptorStatusCode =
                ProtoSuite.Enumerated (Right Proto.RateLimitResponse_CodeOK)
          , Proto.rateLimitResponse_DescriptorStatusCurrentLimit = Nothing
          , Proto.rateLimitResponse_DescriptorStatusLimitRemaining = 0
          }
    )

-- | Unwrap the protobuf representation of a rate limit descriptor.
rateLimitDescriptorFromProto :: Proto.RateLimitDescriptor -> [(RuleKey, RuleValue)]
rateLimitDescriptorFromProto (Proto.RateLimitDescriptor xs) =
    [(RuleKey (TL.toStrict k), RuleValue (TL.toStrict v))
         | Proto.RateLimitDescriptor_Entry k v <- toList xs]
