{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}

-- | Tests for "Fencer.Server".
module Fencer.Server.Test
  ( test_responseNoRules
  )
where

import           BasePrelude

import           Test.Tasty (TestTree, withResource)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import qualified System.Logger as Logger
import qualified Network.GRPC.HighLevel.Generated as Grpc

import           Fencer.AppState
import           Fencer.Server
import qualified Fencer.Proto as Proto

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

-- | Test that when Fencer is started without any rules provided to it (i.e.
-- 'reloadRules' has never been ran), requests to Fencer will error out.
--
-- This behavior matches @lyft/ratelimit@.
test_responseNoRules :: TestTree
test_responseNoRules =
  withResource createServer destroyServer $ \_ ->
    testCase "When no rules have been loaded, all requests error out" $ do
      Grpc.withGRPCClient clientConfig $ \grpcClient -> do
        client <- Proto.rateLimitServiceClient grpcClient
        response <-
          Proto.rateLimitServiceShouldRateLimit client $
            Grpc.ClientNormalRequest request 1 mempty
        case response of
          Grpc.ClientErrorResponse actualError ->
            assertEqual "Got wrong gRPC error response"
              expectedError
              actualError
          Grpc.ClientNormalResponse result _ _ status _ -> do
            assertFailure $
              "Expected an error response, got a normal response: " ++
              "status = " ++ show status ++ ", " ++
              "result = " ++ show result
  where
    -- Sample request.
    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = "domain"
      , Proto.rateLimitRequestDescriptors = mempty
      , Proto.rateLimitRequestHitsAddend = 0
      }

    -- The exact error lyft/ratelimit returns when no rules have been loaded.
    expectedError :: Grpc.ClientError
    expectedError =
      Grpc.ClientIOError
        (Grpc.GRPCIOBadStatusCode
           Grpc.StatusUnknown
           (Grpc.StatusDetails
              "rate limit descriptor list must not be empty"))

----------------------------------------------------------------------------
-- gRPC server
----------------------------------------------------------------------------

-- | Start Fencer on port 50051.
createServer :: IO (Logger.Logger, ThreadId)
createServer = do
  -- TODO: not the best approach. Ideally we should use e.g.
  -- https://hackage.haskell.org/package/tasty-hunit/docs/Test-Tasty-HUnit.html#v:testCaseSteps
  -- but we can't convince @tinylog@ to use the provided step function.
  logger <- Logger.create (Logger.Path "/dev/null")
  appState <- initAppState
  threadId <- forkIO $ runServer logger appState
  pure (logger, threadId)

-- | Kill Fencer.
destroyServer :: (Logger.Logger, ThreadId) -> IO ()
destroyServer (logger, threadId) = do
  Logger.close logger
  killThread threadId

----------------------------------------------------------------------------
-- gRPC client
----------------------------------------------------------------------------

-- | gRPC config that can be used to connect to Fencer started with
-- 'createServer'.
clientConfig :: Grpc.ClientConfig
clientConfig = Grpc.ClientConfig
  { Grpc.clientServerHost = "localhost"
  , Grpc.clientServerPort = 50051
  , Grpc.clientArgs = []
  , Grpc.clientSSLConfig = Nothing
  , Grpc.clientAuthority = Nothing
  }
