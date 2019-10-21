{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE GADTs             #-}

-- | Tests for "Fencer.Server".
module Fencer.Server.Test
  ( test_serverResponseNoRules
  , withServerAppState
  )
where

import           BasePrelude

import           Test.Tasty (TestTree, withResource)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import qualified System.Logger as Logger
import qualified System.IO.Temp as Temp
import qualified Network.GRPC.HighLevel.Generated as Grpc

import           Fencer.AppState
import           Fencer.Server
import           Fencer.Settings (defaultGRPCPort)
import           Fencer.Types (unPort)
import qualified Fencer.Proto as Proto

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

-- | Test that when Fencer is started without any rules provided to it (i.e.
-- 'reloadRules' has never been ran), requests to Fencer will error out.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseNoRules :: TestTree
test_serverResponseNoRules =
  withResource createServer destroyServer $ \_ ->
    testCase "When no rules have been loaded, all requests error out" $ do
      -- NOTE(md): For reasons unkown, the delay in the thread makes a
      -- server test failure for 'test_serverResponseNoRules' go
      -- away. The delay was introduced by assuming it might help
      -- based on the issue comment in gRPC's source code repository:
      -- https://github.com/grpc/grpc/issues/14088#issuecomment-365852100
      --
      -- The length of the delay was fine tuned based on feedback from
      -- test execution.
      --
      -- This delay interacts with the order of the rules and server
      -- tests, which would have executed concurrently by default if
      -- 'after' wasn't used in the Main test module.
      threadDelay 5000 -- 5 ms

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

-- | A type combining a logger, a handle for the logging file, a
-- thread id and an application state.
type LogIdSt = (Logger.Logger, Handle, ThreadId, AppState)

-- | Start Fencer on the default port.
createServer :: IO (Logger.Logger, Handle, ThreadId)
createServer = do
  (logger, logHandle, threadId, _) <- createServerAppState
  pure (logger, logHandle, threadId)

-- | Start Fencer on the default port.
createServerAppState :: IO LogIdSt
createServerAppState = do
  -- TODO: not the best approach. Ideally we should use e.g.
  -- https://hackage.haskell.org/package/tasty-hunit/docs/Test-Tasty-HUnit.html#v:testCaseSteps
  -- but we can't convince @tinylog@ to use the provided step function.

  tmpDir <- Temp.getCanonicalTemporaryDirectory
  -- This opens a temporary file in the ReadWrite mode
  (loggerPath, logHandle) <- Temp.openTempFile tmpDir "fencer-server.log"
  -- The handle has to be closed. Otherwise trying to create a logger
  -- would fail due to a file lock.
  hClose logHandle
  logger <- Logger.create (Logger.Path loggerPath)
  appState <- initAppState
  threadId <- forkIO $ runServer logger appState
  pure (logger, logHandle, threadId, appState)

-- | Kill Fencer.
destroyServer :: (Logger.Logger, Handle, ThreadId) -> IO ()
destroyServer (logger, logHandle, threadId) = do
  Logger.close logger
  hClose logHandle
  killThread threadId

-- | Kill Fencer.
destroyServerAppState :: LogIdSt -> IO ()
destroyServerAppState (logger, logHandle, threadId, _) =
  destroyServer (logger, logHandle, threadId)

-- | Combines starting and destroying a server in a resource-safe
-- manner.
withServerAppState
  :: (IO LogIdSt -> TestTree)
  -> TestTree
withServerAppState =
  withResource createServerAppState destroyServerAppState

----------------------------------------------------------------------------
-- gRPC client
----------------------------------------------------------------------------

-- | gRPC config that can be used to connect to Fencer started with
-- 'createServer'.
clientConfig :: Grpc.ClientConfig
clientConfig = Grpc.ClientConfig
  { Grpc.clientServerHost = "localhost"
  , Grpc.clientServerPort = fromIntegral . unPort $ defaultGRPCPort
  , Grpc.clientArgs = []
  , Grpc.clientSSLConfig = Nothing
  , Grpc.clientAuthority = Nothing
  }
