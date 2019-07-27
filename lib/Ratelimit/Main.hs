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
import qualified Ratelimit.Proto as Proto

data Storage = Storage
    { counters :: !(StmMap.Map CounterKey Counter)
    }

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

-- | Convert a 'CounterStatus' to the protobuf representation.
counterStatusToProto
    :: Maybe CounterStatus
    -> (Proto.RateLimitResponse_Code, Proto.RateLimitResponse_DescriptorStatus)
counterStatusToProto status =
    ( code
    , Proto.RateLimitResponse_DescriptorStatus
          { Proto.rateLimitResponse_DescriptorStatusCode =
                ProtoSuite.Enumerated (Right code)
          , Proto.rateLimitResponse_DescriptorStatusCurrentLimit =
                currentLimit
          , Proto.rateLimitResponse_DescriptorStatusLimitRemaining =
                fromIntegral limitRemaining
          }
    )
  where
    code = case status of
        Nothing -> Proto.RateLimitResponse_CodeOK
        Just CounterStatus{counterHitsOverLimit}
            | counterHitsOverLimit > 0 -> Proto.RateLimitResponse_CodeOVER_LIMIT
            | otherwise -> Proto.RateLimitResponse_CodeOK

    currentLimit = case status of
        Nothing -> Nothing
        Just CounterStatus{counterCurrentLimit} ->
            Just (rateLimitToProto counterCurrentLimit)

    limitRemaining = case status of
        Nothing -> 0
        Just CounterStatus{counterRemainingLimit} -> counterRemainingLimit

-- | Unwrap the protobuf representation of a rate limit descriptor.
rateLimitDescriptorFromProto :: Proto.RateLimitDescriptor -> [(RuleKey, RuleValue)]
rateLimitDescriptorFromProto (Proto.RateLimitDescriptor xs) =
    [(RuleKey (TL.toStrict k), RuleValue (TL.toStrict v))
         | Proto.RateLimitDescriptor_Entry k v <- toList xs]

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
    -- TODO: this doesn't handle leap seconds well. We should handle
    -- leap seconds for (at least) the per-second intervals.
    now <- systemSeconds <$> getSystemTime
    let focus = Focus.lookup >>= \case
            Nothing -> pure Nothing
            Just counter ->
                let (newCounter, status) =
                        updateCounter (#now now) (#hits hits) counter
                in Focus.insert newCounter >> pure (Just status)
    -- TODO: this might retry way too often if we touch too many keys. Need
    -- to figure out whether it's safe to do each operation independently.
    (codes, statuses) <-
        fmap (unzip . map counterStatusToProto) $
        atomically $
        mapM (\key -> StmMap.focus focus key (counters storage)) keys
    let answer = Proto.RateLimitResponse
            { Proto.rateLimitResponseOverallCode =
                  ProtoSuite.Enumerated $
                  Right $
                  if Proto.RateLimitResponse_CodeOVER_LIMIT `elem` codes
                      then Proto.RateLimitResponse_CodeOVER_LIMIT
                      else Proto.RateLimitResponse_CodeOK
            , Proto.rateLimitResponseStatuses =
                  V.fromList statuses
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

main :: IO ()
main = do
    storage <- Storage <$> StmMap.newIO
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit = shouldRateLimit storage }
    let options = Grpc.defaultServiceOptions
    Proto.rateLimitServiceServer handlers options
