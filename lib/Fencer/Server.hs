{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The gRPC server definition.
module Fencer.Server
    ( runServer
    )
where

import BasePrelude

import Control.Concurrent.STM (atomically)
import Named ((:!), arg)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Focus as Focus
import qualified StmContainers.Map as StmMap
import qualified StmContainers.Multimap as StmMultimap
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite
import qualified System.Logger as Logger
import System.Logger (Logger)

import Fencer.Types
import Fencer.AppState
import Fencer.Counter
import Fencer.Time
import Fencer.Rules
import qualified Fencer.Proto as Proto

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

-- | Run the gRPC server serving ratelimit requests.
runServer :: Logger -> AppState -> IO ()
runServer logger appState = do
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit = shouldRateLimit logger appState }
    let options = Grpc.defaultServiceOptions
    Logger.info logger $
        Logger.msg (Logger.val "Starting gRPC server on port 50051")
    Proto.rateLimitServiceServer handlers options

----------------------------------------------------------------------------
-- The "should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Logger
    -> AppState
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit logger appState (Grpc.ServerNormalRequest _metadata request) = do
    -- Decode the protobuf request into our domain types.
    let domain = DomainId (TL.toStrict (Proto.rateLimitRequestDomain request))
    let descriptors :: [[(RuleKey, RuleValue)]]
        descriptors =
            map rateLimitDescriptorFromProto $
            toList (Proto.rateLimitRequestDescriptors request)
    -- Note: 'rateLimitRequestHitsAddend' is 0 (default protobuf value) if
    -- not specified by the caller in the request. We are required to treat
    -- this case as 1 hit.
    let hits :: Word
        hits = max 1 (fromIntegral (Proto.rateLimitRequestHitsAddend request))

    Logger.debug logger $
        Logger.msg (Logger.val "Got rate limit request") .
        Logger.field "domain" (Proto.rateLimitRequestDomain request) .
        Logger.field "descriptors" (show descriptors) .
        Logger.field "hits" hits

    -- Update all counters in one atomic operation, and collect the results.
    --
    -- Note: this might retry often if we touch too many keys, but doing
    -- counter updates independently would be less correct. If two requests
    -- come at the same time and one hits "A" and "B" while the other hits
    -- "B" and "A", and both counters have a rate limit of 1, serving both
    -- requests in parallel might lead to both requests being reported as
    -- "over limit", even though one of them would have succeeded if the
    -- ordering of descriptors was "A", "B" in both requests.
    (codes :: [Proto.RateLimitResponse_Code],
     statuses :: [Proto.RateLimitResponse_DescriptorStatus]) <-
        fmap (unzip . map (maybe ruleNotFoundResponse counterStatusToProto)) $
        atomically $ do
            now <- readTVar (appStateCurrentTime appState)
            forM descriptors $ \descriptor ->
                shouldRateLimitDescriptor
                    appState
                    (#now now)
                    (#hits hits)
                    domain
                    descriptor

    -- Return server response.
    let overallCode =
            if Proto.RateLimitResponse_CodeOVER_LIMIT `elem` codes
                then Proto.RateLimitResponse_CodeOVER_LIMIT
                else Proto.RateLimitResponse_CodeOK
    Logger.debug logger $
        Logger.msg (Logger.val "Replying to the rate limit request") .
        Logger.field "code" (showRateLimitCode overallCode)
    let answer = Proto.RateLimitResponse
            { Proto.rateLimitResponseOverallCode =
                  ProtoSuite.Enumerated (Right overallCode)
            , Proto.rateLimitResponseStatuses = V.fromList statuses
            , Proto.rateLimitResponseHeaders = mempty
            }
    let metadata = mempty
    let statusDetails = ""
    pure $ Grpc.ServerNormalResponse answer metadata Grpc.StatusOk statusDetails

-- | Handle a single descriptor in a 'shouldRateLimit' request.
--
-- Returns the current limit and protobuf-encoded response.
--
-- 'shouldRateLimitDescriptor' will create a new counter if the counter does
-- not exist, or update an existing counter otherwise. The counter will be
-- reset if it has expired, and 'appStateCounterExpiry' will be updated.
shouldRateLimitDescriptor
    :: AppState
    -> "now" :! Timestamp
    -> "hits" :! Word
    -> DomainId
    -> [(RuleKey, RuleValue)]
    -> STM (Maybe (RateLimit, CounterStatus))
shouldRateLimitDescriptor
    appState
    (arg #now -> now)
    (arg #hits -> hits)
    domain
    descriptor
    =
    -- INV001 is satisfied: in the code below we take care to create a new
    -- entry in 'appStateCounterExpiry' when a new counter has been created.
    -- We do not remove counters.
    --
    -- INV002 is satisfied: when 'counterExpiry' changes, we remove the old
    -- counter from 'appStateCounterExpiry' and add the new counter.
    --
    --  INV003 is satisfied: the expiry date 'counterExpiry' is greater than
    -- @now@, and @now@ (passed from the outside) is equal to
    -- 'appStateCurrentTime'.
    StmMap.lookup domain (appStateRules appState) >>= \case
        Nothing -> pure Nothing
        Just ruleTree -> case applyRules descriptor ruleTree of
            Nothing -> pure Nothing
            Just limit -> do
                (mbOldCounter, counter, status) <-
                    StmMap.focus (update limit) key (appStateCounters appState)
                case mbOldCounter of
                    Nothing ->
                        StmMultimap.insert key (counterExpiry counter)
                            (appStateCounterExpiry appState)
                    Just oldCounter ->
                        when (counterExpiry oldCounter /= counterExpiry counter) $ do
                            StmMultimap.delete key (counterExpiry oldCounter)
                                (appStateCounterExpiry appState)
                            StmMultimap.insert key (counterExpiry counter)
                                (appStateCounterExpiry appState)
                pure (Just (limit, status))
  where
    -- Counter key corresponding to our rate limit request.
    key :: CounterKey
    key = CounterKey
        { counterKeyDomain = domain
        , counterKeyDescriptor = descriptor }

    -- Update the counter corresponding to 'key', or create a new counter if
    -- it does not exist. Returns the old counter, the new counter, and
    -- counter status.
    update
        :: RateLimit
        -> Focus.Focus Counter STM (Maybe Counter, Counter, CounterStatus)
    update limit = do
        mbOldCounter <- Focus.lookup
        let (counter, status) =
                updateCounter (#now now) (#hits hits) (#limit limit) $
                    case mbOldCounter of
                        Nothing -> initCounter (#now now) (#limit limit)
                        Just oldCounter -> oldCounter
        Focus.insert counter
        pure (mbOldCounter, counter, status)

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

-- | Show rate limit code as @OK@ or @OVER_LIMIT@.
showRateLimitCode :: Proto.RateLimitResponse_Code -> Text
showRateLimitCode = \case
    Proto.RateLimitResponse_CodeUNKNOWN -> "UNKNOWN"
    Proto.RateLimitResponse_CodeOK -> "OK"
    Proto.RateLimitResponse_CodeOVER_LIMIT -> "OVER_LIMIT"
