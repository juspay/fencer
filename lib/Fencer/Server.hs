{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | The gRPC server definition.
module Fencer.Server
    ( runServer
    )
where

import BasePrelude hiding ((+++))

import           Control.Concurrent.STM (atomically)
import           Control.Monad.Extra (unlessM)
import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite
import qualified System.Logger as Logger
import           System.Logger (Logger)
import           System.Logger.Message ((+++))
import qualified System.Metrics.Gauge as Gauge
import           Named ((:!), arg)

import           Fencer.Logic
import           Fencer.Counter
import qualified Fencer.Proto as Proto
import           Fencer.Types

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

-- | Run the gRPC server serving ratelimit requests.
--
-- TODO: fail if the port is taken? or does it fail already?
runServer
    :: Port -- | E.g. 'defaultGrpcPort'
    -> Logger
    -> "useStatsd" :! Bool
    -> AppState
    -> IO ()
runServer (Port port) logger useStatsd appState = do
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit =
                  shouldRateLimit logger useStatsd appState
            }
    let options = Grpc.defaultServiceOptions
            { Grpc.serverHost = "0.0.0.0"
            , Grpc.serverPort = fromIntegral port
            , Grpc.logger     = Logger.info logger . Logger.msg
            }
    Logger.info logger $
        Logger.msg (("Starting gRPC server at 0.0.0.0:" :: B.ByteString) +++ port)
    Proto.rateLimitServiceServer handlers options

----------------------------------------------------------------------------
-- The "should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Logger
    -> "useStatsd" :! Bool
    -> AppState
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit logger (arg #useStatsd -> useStatsd) appState (Grpc.ServerNormalRequest serverCall request) = do
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

    -- Check some conditions and throw errors if necessary.
    let cancelWithError :: String -> IO ()
        cancelWithError = Grpc.serverCallCancel serverCall Grpc.StatusUnknown

    unlessM (atomically (getAppStateRulesLoaded appState)) $ do
        Logger.info logger $
            Logger.msg (Logger.val "Rules not loaded, responding with an error")
        cancelWithError "no rate limit configuration loaded"

    when (domain == DomainId "") $ do
        Logger.info logger $
            Logger.msg (Logger.val "Empty domain ID, responding with an error")
        cancelWithError "rate limit domain must not be empty"

    when (null @[] descriptors) $ do
        Logger.info logger $
            Logger.msg (Logger.val "Empty descriptor list, responding with an error")
        cancelWithError "rate limit descriptor list must not be empty"

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
        do results :: [Maybe (RuleLeaf, CounterStatus)] <-
               atomically $ forM descriptors $ \descriptor ->
                   updateLimitCounter appState (#hits hits) domain descriptor
           when useStatsd $
               forM_ (catMaybes results) $ \(leaf, status) -> do
                   let key = ruleLeafStatsKey leaf
                   atomically (getStatsForKey appState key) >>= \case
                       Nothing -> pure ()
                       Just stats -> do
                           Logger.debug logger $
                               Logger.msg (Logger.val "Reporting to statsd") .
                               Logger.field "key" (show key)
                           Gauge.add (statsTotalHits stats) $
                               fromIntegral hits
                           Gauge.add (statsOverLimit stats) $
                               fromIntegral (counterHitsOverLimit status)
           pure $ unzip $ map (maybe ruleNotFoundResponse counterStatusToProto) results

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

-- | Convert a 'CounterStatus' to the protobuf representation.
counterStatusToProto
    :: (RuleLeaf, CounterStatus)
    -> (Proto.RateLimitResponse_Code, Proto.RateLimitResponse_DescriptorStatus)
counterStatusToProto (leaf, status) =
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
    limit = ruleLeafLimit leaf
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
