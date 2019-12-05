{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | The gRPC server definition.
module Fencer.Server
    ( runServer
    , runServerDefaultPort
    )
where

import BasePrelude hiding ((+++))

import           Control.Concurrent.STM (atomically)
import           Control.Monad.Extra (unlessM)
import qualified Data.ByteString.Char8 as B
import           Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite
import qualified System.Logger as Logger
import           System.Logger (Logger)
import           System.Logger.Message ((+++))

import           Fencer.Logic
import           Fencer.Counter
import qualified Fencer.Proto as Proto
import           Fencer.Settings
import           Fencer.Types

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

-- | Run the gRPC server serving ratelimit requests.
--
-- TODO: fail if the port is taken? or does it fail already?
runServer :: Settings -> Logger -> AppState -> IO ()
runServer settings logger appState = do
    let port = unPort $ settingsGRPCPort settings
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit =
                  shouldRateLimit settings logger appState
            }
    let options = Grpc.defaultServiceOptions
            { Grpc.serverHost = "0.0.0.0"
            , Grpc.serverPort = fromIntegral port
            , Grpc.logger     = Logger.info logger . Logger.msg
            }
    Logger.info logger $
        Logger.msg (("Starting gRPC server at 0.0.0.0:" :: B.ByteString) +++ port)
    Proto.rateLimitServiceServer handlers options

-- | Run the gRPC server serving ratelimit requests on the default
-- port.
runServerDefaultPort :: Settings -> Logger -> AppState -> IO ()
runServerDefaultPort settings =
    runServer (settings {settingsGRPCPort = defaultGRPCPort})

----------------------------------------------------------------------------
-- The "should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Settings
    -> Logger
    -> AppState
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit settings logger appState (Grpc.ServerNormalRequest serverCall request) = do
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
    let
      showKeyValue :: (RuleKey, RuleValue) -> String
      showKeyValue (RuleKey k, RuleValue v) = unpack k ++ case v of
        "" -> ""
        _  -> "_" ++ unpack v
    limitsStatuses :: [Maybe (RateLimit, CounterStatus)] <- atomically $
      forM descriptors $ \descriptor ->
        updateLimitCounter appState (#hits hits) domain descriptor
    limitsStatusesCounters :: [Maybe (RateLimit, CounterStatus, Counter)] <- atomically $
      forM (descriptors `zip` limitsStatuses) $ \(descriptor, limitStatus) -> do
        case limitStatus of
          Nothing -> pure Nothing
          Just (limit, status) -> do
            let counterKey = CounterKey
                  { counterKeyDomain = domain
                  , counterKeyDescriptor = descriptor
                  , counterKeyUnit = rateLimitUnit limit
                  }
            getCounter appState counterKey >>= \case
              Nothing -> pure Nothing
              Just c  -> pure $ Just (limit, status, c)

    let
      -- | Compute the near limit statistic based on a rate limit and
      -- a counter status.
      statNearLimit
        :: RateLimit
        -> CounterStatus
        -> Word
      statNearLimit rateLimit status =
        let
          limit = rateLimitRequestsPerUnit rateLimit
          nearRatio = settingsNearLimitRatio settings
          threshold = round $
            fromIntegral limit * nearRatio
          remaining = counterRemainingLimit status
         in if counterHitsOverLimit status /= 0
           then 0
           else if remaining + threshold > limit
             then 0
             else limit - threshold - remaining
      -- | Log the three statistics: near limit, over limit and total
      -- hits.
      logStats
        :: [(RuleKey, RuleValue)]
        -> Maybe (RateLimit, CounterStatus, Counter)
        -> IO ()
      logStats _     Nothing            = pure ()
      logStats descs (Just (limit, status, counter)) = do
        let
          prefix = B.pack $
            unpack ("fencer.service.rate_limit." <> (unDomainId domain)) <>
            "." <>
            (intercalate "." . fmap showKeyValue $ descs) <> "."
        forM_
          [ ("near_limit", statNearLimit limit status)
          , ("over_limit", counterHitsOverLimit status)
          , ("total_hits", counterHits counter) ] $ \(s, m) -> do
            Logger.info logger $ Logger.msg $ Logger.val $
              prefix <> s <> ": " <> (B.pack . show $ m)

    if settingsUseStatsd settings
      then pure () -- TODO(md): implement sending statistics to the statsd
      else forM_ (descriptors `zip` limitsStatusesCounters) $ uncurry logStats

    (codes :: [Proto.RateLimitResponse_Code],
     statuses :: [Proto.RateLimitResponse_DescriptorStatus]) <- fmap
         (unzip . map (maybe ruleNotFoundResponse counterStatusToProto))
         (pure limitsStatuses)

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
