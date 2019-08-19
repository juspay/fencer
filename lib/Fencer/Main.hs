{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The main module of Fencer, included in the library to make it easier to
-- load it in REPL. @src/Main.hs@ simply calls 'main' from this module.
module Fencer.Main
    ( main
    )
where

import BasePrelude
import Data.Hashable (Hashable)
import Control.Concurrent.STM (atomically, orElse)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Named
import System.Directory (listDirectory, doesFileExist)
import System.FilePath
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Focus as Focus
import qualified StmContainers.Map as StmMap
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite
import qualified Data.HashMap.Strict as HM
import qualified Data.Yaml as Yaml

import Fencer.Types
import Fencer.Counter
import Fencer.Time
import Fencer.Match
import qualified Fencer.Proto as Proto
import Fencer.Settings

----------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------

-- | Load YAML rules from @config/@, create in-memory state and run the gRPC
-- server serving ratelimit requests.
main :: IO ()
main = do
    storage <- newStorage
    settings <- getSettingsFromEnvironment
    let configDir = settingsRoot settings </> settingsSubdirectory settings </> "config"
    rules <- parseRules
        (#directory configDir)
        (#ignoreDotFiles (settingsIgnoreDotFiles settings))
    atomically $ forM_ rules $ \rule -> do
        let domain = domainDefinitionId rule
            tree = makeRuleTree (domainDefinitionDescriptors rule)
        StmMap.insert tree domain (storageRules storage)
    let handlers = Proto.RateLimitService
            { Proto.rateLimitServiceShouldRateLimit = shouldRateLimit storage }
    let options = Grpc.defaultServiceOptions
    Proto.rateLimitServiceServer handlers options

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------

-- | In-memory storage for the state of the program.
data Storage = Storage
    { storageRules :: !(StmMap.Map DomainId RuleTree)
    , storageCounters :: !(StmMap.Map CounterKey Counter)
    }

-- | Create an empty 'Storage'.
newStorage :: IO Storage
newStorage = Storage <$> StmMap.newIO <*> StmMap.newIO

----------------------------------------------------------------------------
-- Load rules
----------------------------------------------------------------------------

-- | Gather rate limiting rules (*.yml, *.yaml) from a directory.
-- Subdirectories are not included.
--
-- Throws an exception for unparseable or unreadable files.
parseRules
  :: "directory" :! FilePath
  -> "ignoreDotFiles" :! Bool  -- ^ Ignore hidden files (starting with a dot)
  -> IO [DomainDefinition]
parseRules (arg #directory -> directory) (arg #ignoreDotFiles -> ignoreDotFiles) = do
    files <- filterM doesFileExist . map (directory </>) =<< listDirectory directory
    let ruleFiles =
            (if ignoreDotFiles then filter (not . isDotFile) else id) $
            filter isYaml files
    mapM Yaml.decodeFileThrow ruleFiles
    -- TODO: what does lyft/ratelimit do with unparseable files?
  where
    isYaml :: FilePath -> Bool
    isYaml file = takeExtension file `elem` [".yml", ".yaml"]

    isDotFile :: FilePath -> Bool
    isDotFile file = "." `isPrefixOf` takeFileName file

----------------------------------------------------------------------------
-- "Should rate limit" method
----------------------------------------------------------------------------

-- | gRPC handler for the "should rate limit?" method.
shouldRateLimit
    :: Storage
    -> Grpc.ServerRequest 'Grpc.Normal Proto.RateLimitRequest Proto.RateLimitResponse
    -> IO (Grpc.ServerResponse 'Grpc.Normal Proto.RateLimitResponse)
shouldRateLimit storage (Grpc.ServerNormalRequest _metadata request) = do
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

    -- Execute the query.
    now <- getTimestamp
    -- TODO: this might retry way too often if we touch too many keys. Need
    -- to figure out whether it's safe to do each operation independently.
    (codes :: [Proto.RateLimitResponse_Code],
     statuses :: [Proto.RateLimitResponse_DescriptorStatus]) <-
        fmap (unzip . map (maybe ruleNotFoundResponse counterStatusToProto)) $
        atomically $
        forM descriptors $ \descriptor ->
            shouldRateLimitDescriptor storage (#now now) (#hits hits) domain descriptor
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

    -- Return server response.
    let metadata = mempty
        statusDetails = ""
    pure $ Grpc.ServerNormalResponse answer metadata Grpc.StatusOk statusDetails

-- | Handle a single descriptor in a 'shouldRateLimit' request.
--
-- Returns the current limit and protobuf-encoded response.
shouldRateLimitDescriptor
    :: Storage
    -> "now" :! Timestamp
    -> "hits" :! Word
    -> DomainId
    -> [(RuleKey, RuleValue)]
    -> STM (Maybe (RateLimit, CounterStatus))
shouldRateLimitDescriptor
    storage
    (arg #now -> now)
    (arg #hits -> hits)
    domain
    descriptor
    =
    StmMap.lookup domain (storageRules storage) >>= \case
        Nothing -> pure Nothing
        Just ruleTree -> case matchRequest descriptor ruleTree of
            Nothing ->
                pure Nothing
            Just limit ->
                Just <$> StmMap.focus (update limit) key (storageCounters storage)
  where
    -- Counter key corresponding to our rate limit request.
    key :: CounterKey
    key = CounterKey
        { counterKeyDomain = domain
        , counterKeyDescriptor = descriptor }

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
