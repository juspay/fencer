{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}

-- | Tests for "Fencer.Server".
module Fencer.Server.Test
  ( test_responseInvalidConfig
  )
where

import           BasePrelude

import           Test.Tasty (TestTree, withResource)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import qualified System.Logger as Logger
import qualified Network.GRPC.HighLevel.Generated as Grpc
import qualified Proto3.Suite.Types as ProtoSuite

import           Fencer.AppState
import           Fencer.Server
import qualified Fencer.Proto as Proto

clientConfig :: Grpc.ClientConfig
clientConfig = Grpc.ClientConfig
  { Grpc.clientServerHost = "localhost"
  , Grpc.clientServerPort = 50051
  , Grpc.clientArgs = []
  , Grpc.clientSSLConfig = Nothing
  , Grpc.clientAuthority = Nothing
  }

test_responseInvalidConfig :: TestTree
test_responseInvalidConfig =
  withResource createServer destroyServer $ \_ ->
    testCase "When no rules have been loaded, all requests return ERROR" $ do
      -- TODO newline in stderr
      Grpc.withGRPCClient clientConfig $ \grpcClient -> do
        client <- Proto.rateLimitServiceClient grpcClient
        let request = Proto.RateLimitRequest
              { Proto.rateLimitRequestDomain = "domain"
              , Proto.rateLimitRequestDescriptors = mempty
              , Proto.rateLimitRequestHitsAddend = 0
              }
        response <-
          Proto.rateLimitServiceShouldRateLimit client $
          Grpc.ClientNormalRequest request 1 mempty
        case response of
          Grpc.ClientErrorResponse err ->
            assertFailure ("ClientErrorResponse: " <> show err)
          Grpc.ClientNormalResponse result _ _ status _ -> do
            assertEqual "wrong status TODO better message"
              Grpc.StatusUnknown -- is this the right status?
              status
            assertEqual "result"
              Proto.RateLimitResponse
                { Proto.rateLimitResponseOverallCode =
                    ProtoSuite.Enumerated (Right Proto.RateLimitResponse_CodeUNKNOWN)
                , Proto.rateLimitResponseStatuses = mempty
                , Proto.rateLimitResponseHeaders = mempty
                }
              result

  where
    createServer :: IO (Logger.Logger, ThreadId)
    createServer = do
      logger <- Logger.create Logger.StdErr
      appState <- initAppState
      threadId <- forkIO $ runServer logger appState
      pure (logger, threadId)

    destroyServer :: (Logger.Logger, ThreadId) -> IO ()
    destroyServer (logger, threadId) = do
      Logger.close logger
      killThread threadId
