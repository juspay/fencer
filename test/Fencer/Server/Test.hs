{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs             #-}

-- | Tests for "Fencer.Server".
module Fencer.Server.Test
  ( tests
  , withServer
  , serverAppState
  )
where

import           BasePrelude

import           Test.Tasty (TestTree, testGroup, withResource)
import           Test.Tasty.HUnit (assertEqual, assertFailure, testCase, Assertion)
import qualified System.Logger as Logger
import qualified System.IO.Temp as Temp
import qualified Network.GRPC.HighLevel.Generated as Grpc
import           Data.ByteString (ByteString)
import           GHC.Exts (fromList)

import           Fencer.Logic
import           Fencer.Server
import           Fencer.Settings (defaultGRPCPort, getLogLevel, newLogger)
import           Fencer.Types (unPort)
import qualified Fencer.Proto as Proto

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Server tests" [ test_serverResponseNoRules ]


-- | Test that when Fencer is started without any rules provided to it (i.e.
-- 'reloadRules' has never been ran), requests to Fencer will error out.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseNoRules :: TestTree
test_serverResponseNoRules =
  withResource createServer destroyServer $ \_ ->
    testCase "When no rules have been loaded, all requests error out" $ do
      Grpc.withGRPCClient clientConfig $ \grpcClient -> do
        client <- Proto.rateLimitServiceClient grpcClient
        response <-
          Proto.rateLimitServiceShouldRateLimit client $
            Grpc.ClientNormalRequest request 1 mempty
        expectError
          (unknownError "no rate limit configuration loaded")
          response
  where
    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = "domain"
      , Proto.rateLimitRequestDescriptors =
          fromList $
          [ Proto.RateLimitDescriptor $
              fromList [Proto.RateLimitDescriptor_Entry "key" "value"]
          ]
      , Proto.rateLimitRequestHitsAddend = 0
      }

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Assert that a gRPC request is successful and has a specific result and
-- status code.
expectSuccess
  :: (Eq result, Show result)
  => (result, Grpc.StatusCode)
  -> Grpc.ClientResult 'Grpc.Normal result
  -> Assertion
expectSuccess expected actual = case actual of
  Grpc.ClientErrorResponse actualError ->
    assertFailure $
      "Expected a normal response, got an error response: " ++
      show actualError
  Grpc.ClientNormalResponse result _ _ status _ ->
    assertEqual "Got wrong response" expected (result, status)

-- | Assert that a gRPC request is unsuccessful.
expectError
  :: Show result
  => Grpc.ClientError
  -> Grpc.ClientResult 'Grpc.Normal result
  -> Assertion
expectError expected actual = case actual of
  Grpc.ClientErrorResponse actualError ->
    assertEqual "Got wrong gRPC error response" expected actualError
  Grpc.ClientNormalResponse result _ _ status _ ->
    assertFailure $
      "Expected an error response, got a normal response: " ++
      "status = " ++ show status ++ ", " ++
      "result = " ++ show result

-- | A constructor for the specific flavor of gRPC errors returned by Fencer.
unknownError :: ByteString -> Grpc.ClientError
unknownError err =
  Grpc.ClientIOError
    (Grpc.GRPCIOBadStatusCode Grpc.StatusUnknown (Grpc.StatusDetails err))

----------------------------------------------------------------------------
-- gRPC server
----------------------------------------------------------------------------

-- | A type combining a logger, a handle for the logging file, a
-- thread id and an application state.
data Server = Server
  { serverLogger    :: Logger.Logger
  , serverLogHandle :: Handle
  , serverThreadId  :: ThreadId
  , serverAppState  :: AppState
  }

-- | Start Fencer on the default port.
createServer :: IO Server
createServer = do
  -- TODO: not the best approach. Ideally we should use e.g.
  -- https://hackage.haskell.org/package/tasty-hunit/docs/Test-Tasty-HUnit.html#v:testCaseSteps
  -- but we can't convince @tinylog@ to use the provided step function.

  tmpDir <- Temp.getCanonicalTemporaryDirectory
  -- This opens a temporary file in the ReadWrite mode
  (loggerPath, serverLogHandle) <- Temp.openTempFile tmpDir "fencer-server.log"
  -- The handle has to be closed. Otherwise trying to create a logger
  -- would fail due to a file lock.
  hClose serverLogHandle
  serverLogger   <- getLogLevel >>= newLogger (Logger.Path loggerPath)
  serverAppState <- initAppState
  serverThreadId <- forkIO $ runServer serverLogger serverAppState

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

  pure Server{..}

-- | Kill Fencer.
destroyServer :: Server -> IO ()
destroyServer server =
  Logger.close (serverLogger server)
    `finally` hClose (serverLogHandle server)
    `finally` killThread (serverThreadId server)

-- | Combines starting and destroying a server in a resource-safe
-- manner.
withServer
  :: (IO Server -> TestTree)
  -> TestTree
withServer =
  withResource createServer destroyServer

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
