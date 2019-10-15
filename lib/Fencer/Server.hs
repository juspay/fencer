{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The gRPC server definition.
module Fencer.Server
    ( runServer
    , runServerWithPort
    )
where

import BasePrelude

import Control.Concurrent.STM (atomically)
import Named ((:!), arg)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite
import qualified System.Logger as Logger
import System.Logger (Logger)

import Fencer.Types
import Fencer.AppState
import Fencer.Counter
import qualified Fencer.Proto as Proto

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

-- | Run the gRPC server serving ratelimit requests.
--
-- TODO: fail if the port is taken? or does it fail already?
runServerWithPort :: Port -> Logger -> AppState -> IO ()
runServerWithPort (Port port) logger appState = do
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit = shouldRateLimit logger appState
            }
    let options = Grpc.defaultServiceOptions
            { Grpc.serverHost = "0.0.0.0"
            , Grpc.serverPort = fromIntegral port
              -- TODO: set the logger
            }
    Logger.info logger $
        Logger.msg (Logger.val "Starting gRPC server at 0.0.0.0:50051")
    Proto.rateLimitServiceServer handlers options

-- | Run the gRPC server serving ratelimit requests on the default
-- 50051 port.
runServer :: Logger -> AppState -> IO ()
runServer = runServerWithPort (Port 50051)

----------------------------------------------------------------------------
-- The "should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Logger
    -> AppState
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit logger appState (Grpc.ServerNormalRequest serverCall request) = do
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

    rulesLoaded <- atomically $ getAppStateRulesLoaded appState
    unless rulesLoaded $ do
        Logger.info logger $
            Logger.msg (Logger.val "Rules not loaded, responding with an error")
        Grpc.serverCallCancel
            serverCall
            Grpc.StatusUnknown
            "rate limit descriptor list must not be empty"

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
        atomically $ forM descriptors $ \descriptor ->
            shouldRateLimitDescriptor appState (#hits hits) domain descriptor

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
            -- The headers field is never set, as per
            -- https://github.com/lyft/ratelimit/blob/883e9705856eb8c891813589f95809dbb2bbec39/src/service/ratelimit.go#L120-L131
            , Proto.rateLimitResponseHeaders = mempty
            }
    let metadata = mempty
    let statusDetails = ""
    pure $ Grpc.ServerNormalResponse answer metadata Grpc.StatusOk statusDetails

-- | Handle a single descriptor in a 'shouldRateLimit' request.
--
-- Returns the current limit and response.
--
-- 'shouldRateLimitDescriptor' will create a new counter if the counter does
-- not exist, or update an existing counter otherwise. The counter will be
-- reset if it has expired, and 'appStateCounterExpiry' will be updated.
shouldRateLimitDescriptor
    :: AppState
    -> "hits" :! Word
    -> DomainId
    -> [(RuleKey, RuleValue)]
    -> STM (Maybe (RateLimit, CounterStatus))
shouldRateLimitDescriptor appState (arg #hits -> hits) domain descriptor =
    getLimit appState domain descriptor >>= \case
        Nothing -> pure Nothing
        Just limit -> do
            let counterKey :: CounterKey
                counterKey = CounterKey
                  { counterKeyDomain = domain
                  , counterKeyDescriptor = descriptor
                  , counterKeyUnit = rateLimitUnit limit }
            status <- recordHits appState (#hits hits) (#limit limit) counterKey
            pure (Just (limit, status))

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
