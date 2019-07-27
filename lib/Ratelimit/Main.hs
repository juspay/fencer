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
import qualified Ratelimit.Proto as Proto

data CounterKey = CounterKey
    { counterKeyDomain :: !DomainId
    , counterKeyDescriptor :: ![(RuleKey, RuleValue)]
    }
    deriving stock (Eq, Generic)
    deriving anyclass (Hashable)

data Storage = Storage
    { counters :: !(StmMap.Map CounterKey Counter)
    }

data Counter = Counter
    { counterHits :: !Word
    , counterSlot :: !Word
    , counterLimit :: !RateLimit
    }

-- | Verdict returned from the rate limiter.
data HitVerdict
    = HitStatus
          { -- | The current limit, returned for convenience.
            statusCurrentLimit :: !RateLimit
            -- | How many hits can be taken before the limit is reached.
            -- Will be 0 if the limit has been reached already.
          , statusRemainingLimit :: !Word
            -- | How many hits went over limit. Will be 0 if the limit has
            -- not been reached.
          , statusHitsOverLimit :: !Word
          }
    | HitUnknownRule

-- | Handle a request to the rate limiter: increment the relevant counter
-- and return the verdict.
--
-- If the counter does not exist, 'hit' will fail with 'HitUnknownRule'. Our
-- invariant is that all rate limiting rules that we know of have exactly
-- one counter corresponding to them. If the counter does not exist, the
-- rule does not exist.
--
-- The counter is always incremented, even if the limit has been reached.
-- This matches the behavior of @lyft/ratelimit@.
hit
    :: "key" :! CounterKey
    -> "hits" :! Word
    -> Storage
    -> IO HitVerdict
hit (arg #key -> key) (arg #hits -> hits) storage = do
    -- TODO: this doesn't handle leap seconds well. We should handle
    -- leap seconds for (at least) the per-second intervals.
    now <- systemSeconds <$> getSystemTime
    atomically $ StmMap.focus (go now) key (counters storage)
  where
    -- Increment the counter for the 'key', given current time.
    go :: Int64 -> Focus.Focus Counter STM HitVerdict
    go now = Focus.lookup >>= \case
        Nothing -> pure HitUnknownRule
        Just counter -> do
            let limit = rateLimitRequestsPerUnit (counterLimit counter)
                currentSlot =
                    fromIntegral $
                    now `div` timeUnitToSeconds (rateLimitUnit (counterLimit counter))
            if | currentSlot > counterSlot counter -> do
                     -- The counter is outdated, create a new one.
                     --
                     -- Note that we only create a new counter if the if the
                     -- counter time slot is /older/ than our request's time
                     -- slot. If it's newer, we can simply pretend that the
                     -- request came later than it actually did.
                     Focus.insert $ counter
                         { counterHits = hits
                         , counterSlot = currentSlot }
                     pure $ if hits <= limit
                         then HitStatus
                                  { statusCurrentLimit = counterLimit counter
                                  , statusRemainingLimit = limit - hits
                                  , statusHitsOverLimit = 0 }
                         else HitStatus
                                  { statusCurrentLimit = counterLimit counter
                                  , statusRemainingLimit = 0
                                  , statusHitsOverLimit = hits - limit }
               | otherwise -> do
                     -- Increment the counter.
                     let newHits = counterHits counter + hits
                     Focus.insert $ counter
                         { counterHits = newHits }
                     -- Take care to count the hits correctly: if the
                     -- counter was already over limit, we should report
                     -- only the new hits as being over limit, i.e. we are
                     -- allowed to report at most 'hits' hits.
                     pure $ if newHits <= limit
                         then HitStatus
                                  { statusCurrentLimit = counterLimit counter
                                  , statusRemainingLimit = limit - newHits
                                  , statusHitsOverLimit = 0 }
                         else HitStatus
                                  { statusCurrentLimit = counterLimit counter
                                  , statusRemainingLimit = 0
                                  , statusHitsOverLimit = min hits (newHits - limit) }

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

-- | Convert a 'HitVerdict' to the protobuf representation.
hitVerdictToProto :: HitVerdict -> Proto.RateLimitResponse
hitVerdictToProto verdict =
    Proto.RateLimitResponse
        { Proto.rateLimitResponseOverallCode =
              ProtoSuite.Enumerated (Right code)
        , Proto.rateLimitResponseStatuses =
              V.singleton $
              Proto.RateLimitResponse_DescriptorStatus
                    { Proto.rateLimitResponse_DescriptorStatusCode =
                          ProtoSuite.Enumerated (Right code)
                    , Proto.rateLimitResponse_DescriptorStatusCurrentLimit =
                          currentLimit
                    , Proto.rateLimitResponse_DescriptorStatusLimitRemaining =
                          fromIntegral limitRemaining
                    }
        }
  where
    code = case verdict of
        HitUnknownRule -> Proto.RateLimitResponse_CodeOK
        HitStatus{statusHitsOverLimit}
            | statusHitsOverLimit > 0 -> Proto.RateLimitResponse_CodeOVER_LIMIT
            | otherwise -> Proto.RateLimitResponse_CodeOK

    currentLimit = case verdict of
        HitUnknownRule -> Nothing
        HitStatus{statusCurrentLimit} -> Just (rateLimitToProto statusCurrentLimit)

    limitRemaining = case verdict of
        HitUnknownRule -> 0
        HitStatus{statusRemainingLimit} -> statusRemainingLimit

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Storage
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit storage (Grpc.ServerNormalRequest _metadata request) = do
    let domain = DomainId (TL.toStrict (Proto.rateLimitRequestDomain request))
    -- TODO: we assume that the request contains exactly one descriptor. We
    -- should figure out what to do if it's more than one.
    let descriptor = case toList (Proto.rateLimitRequestDescriptors request) of
            [Proto.RateLimitDescriptor ds] ->
                [(RuleKey (TL.toStrict k), RuleValue (TL.toStrict v))
                    | Proto.RateLimitDescriptor_Entry k v <- toList ds]
            otherwise -> error "expected exactly one descriptor"
    let key = CounterKey
            { counterKeyDomain = domain
            , counterKeyDescriptor = descriptor }
    -- Note: 'rateLimitRequestHitsAddend' could be 0 (default value) if not
    -- specified by the caller in the request.
    let hits :: Word
        hits = max 1 (fromIntegral (Proto.rateLimitRequestHitsAddend request))
    -- Record hits, get a verdict, and return it.
    verdict <- hit (#key key) (#hits hits) storage
    pure $ Grpc.ServerNormalResponse
        -- answer
        (hitVerdictToProto verdict)
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
