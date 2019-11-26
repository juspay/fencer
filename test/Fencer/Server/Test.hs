{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
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

import           Data.ByteString (ByteString)
import qualified Data.Vector as Vector
import           GHC.Exts (fromList)
import           Named ((:!), arg)
import qualified Network.GRPC.HighLevel.Generated as Grpc
import           Proto3.Suite.Types (Enumerated(..))
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.Logger as Logger
import qualified System.IO.Temp as Temp
import           Test.Tasty (TestTree, testGroup, withResource)
import           Test.Tasty.HUnit (HasCallStack, assertEqual, assertFailure, testCase, Assertion)

import           Fencer.Logic
import           Fencer.Server
import           Fencer.Settings (defaultGRPCPort, getLogLevel, newLogger)
import           Fencer.Types
import           Fencer.Rules
import           Fencer.Rules.Test.Examples
                 ( domainDescriptorKeyValueText
                 , domainDescriptorKeyText
                 , domainDescriptorKeyValue
                 , duplicateRuleDomain
                 )
import           Fencer.Rules.Test.Helpers (writeAndLoadRules)
import           Fencer.Rules.Test.Types (RuleFile(..), simpleRuleFile)
import qualified Fencer.Proto as Proto

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

----------------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Server tests"
  [ test_serverResponseNoRules
  , test_serverResponseEmptyDomain
  , test_serverResponseEmptyDescriptorList
  , test_serverResponseReadPermissions
  , test_serverResponseDuplicateDomain
  , test_serverResponseDuplicateRule
  ]

-- | Test that when Fencer is started without any rules provided to it (i.e.
-- 'reloadRules' has never been ran), requests to Fencer will error out.
--
-- This test also corresponds to a case when there is faulty
-- configuration and Fencer loads no rules at startup. In such a case
-- the server responds to requests with an error.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseNoRules :: TestTree
test_serverResponseNoRules =
  withResource createServer destroyServer $ \serverIO ->
    testCase "When no rules have been loaded, all requests error out" $ do
      server <- serverIO
      withService server $ \service -> do
        response <- Proto.rateLimitServiceShouldRateLimit service $
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

-- | Test that requests with an empty domain name result in an error.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseEmptyDomain :: TestTree
test_serverResponseEmptyDomain =
  withResource createServer destroyServer $ \serverIO ->
    testCase "Requests with an empty domain name result in an error" $ do
      server <- serverIO
      atomically (setRules (serverAppState server) rules)
      withService server $ \service -> do
        response <- Proto.rateLimitServiceShouldRateLimit service $
          Grpc.ClientNormalRequest request 1 mempty
        expectError
          (unknownError "rate limit domain must not be empty")
          response
  where
    rules :: [(DomainId, RuleTree)]
    rules = [domainToRuleTree domainDefinitionWithoutRules]

    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = ""
      , Proto.rateLimitRequestDescriptors =
          fromList $
          [ Proto.RateLimitDescriptor $
              fromList [Proto.RateLimitDescriptor_Entry "key" "value"]
          ]
      , Proto.rateLimitRequestHitsAddend = 0
      }

-- | Test that requests with an empty descriptor list result in an error.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseEmptyDescriptorList :: TestTree
test_serverResponseEmptyDescriptorList =
  withResource createServer destroyServer $ \serverIO ->
    testCase "Requests with an empty descriptor list result in an error" $ do
      server <- serverIO
      atomically (setRules (serverAppState server) rules)
      withService server $ \service -> do
        response <- Proto.rateLimitServiceShouldRateLimit service $
          Grpc.ClientNormalRequest request 1 mempty
        expectError
          (unknownError "rate limit descriptor list must not be empty")
          response
  where
    rules :: [(DomainId, RuleTree)]
    rules = [domainToRuleTree domainDefinitionWithoutRules]

    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = "domain"
      , Proto.rateLimitRequestDescriptors = mempty
      , Proto.rateLimitRequestHitsAddend = 0
      }

-- | Test that a request with a non-empty descriptor list results in an
-- OK response in presence of a configuration file without read
-- permissions.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseReadPermissions :: TestTree
test_serverResponseReadPermissions =
  withResource createServer destroyServer $ \serverIO ->
    testCase "OK response with one YAML file without read permissions" $
      Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
        server <- serverIO
        writeAndLoadRules
          (#ignoreDotFiles False)
          (#root tempDir)
          (#files files)
          >>= \case
          Left _ -> assertFailure "Failed to load a valid domain!"
          Right rules -> do
            atomically $
              setRules (serverAppState server) (domainToRuleTree <$> rules)
            withService server $ \service -> do
              response <- Proto.rateLimitServiceShouldRateLimit service $
                Grpc.ClientNormalRequest request 1 mempty
              expectSuccess
                (genericOKResponse, Grpc.StatusOk)
                response
  where
    files :: [RuleFile]
    files =
      [ MkRuleFile
          ("domain1" </> "config.yml")
          domainDescriptorKeyValueText
          (const Dir.emptyPermissions)
      , simpleRuleFile
          ("domain2" </> "config" </> "config.yml")
          domainDescriptorKeyText
      ]

    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = "domain"
      , Proto.rateLimitRequestDescriptors =
          fromList $
          [ Proto.RateLimitDescriptor $
              fromList [Proto.RateLimitDescriptor_Entry "key" ""]
          ]
      , Proto.rateLimitRequestHitsAddend = 0
      }

-- | A parameterized test that checks if a request with a non-empty
-- descriptor list results in a response with an unknown status code
-- in presence of a configuration with a duplicate domain/rule.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseDuplicateDomainOrRule
  :: "label" :! String
  -> "definitionsOrFiles" :! Either [DomainDefinition] [RuleFile]
  -> TestTree
test_serverResponseDuplicateDomainOrRule
  (arg #label -> label)
  (arg #definitionsOrFiles -> definitionsOrFiles) =
  withResource createServer destroyServer $ \serverIO ->
    testCase ("In presence of duplicate " ++ label ++ " all requests error") $
      Temp.withSystemTempDirectory "fencer-config" $ \tempDir -> do
        server <- serverIO
        df :: Either (NonEmpty LoadRulesError) [DomainDefinition] <-
          case definitionsOrFiles of
            Left domains ->
              pure (validatePotentialDomains $ Right . Just <$> domains)
            Right files  ->
              writeAndLoadRules
                (#ignoreDotFiles False)
                (#root tempDir)
                (#files files)
        case df of
          Left _ ->
            withService server $ \service -> do
              response <- Proto.rateLimitServiceShouldRateLimit service $
                Grpc.ClientNormalRequest request 1 mempty
              expectError
                (unknownError "no rate limit configuration loaded")
                response
          Right _ -> assertFailure $
            "Expected a failure, and got domain definitions instead"
  where
    request :: Proto.RateLimitRequest
    request = Proto.RateLimitRequest
      { Proto.rateLimitRequestDomain = "domain1"
      , Proto.rateLimitRequestDescriptors =
          fromList $
          [ Proto.RateLimitDescriptor $
              fromList [Proto.RateLimitDescriptor_Entry "some key" ""]
          ]
      , Proto.rateLimitRequestHitsAddend = 0
      }

-- | Test that a request with a non-empty descriptor list results in a
-- response with an unknown status code in presence of a configuration
-- with a duplicate domain.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseDuplicateDomain :: TestTree
test_serverResponseDuplicateDomain =
  test_serverResponseDuplicateDomainOrRule
    (#label "domains")
    (#definitionsOrFiles (Left $ replicate 2 domainDescriptorKeyValue))

-- | Test that a request with a non-empty descriptor list results in a
-- response with an unknown status code in presence of a configuration
-- with a duplicate rule.
--
-- This behavior matches @lyft/ratelimit@.
test_serverResponseDuplicateRule :: TestTree
test_serverResponseDuplicateRule =
  test_serverResponseDuplicateDomainOrRule
    (#label "rules")
    (#definitionsOrFiles
       (Right [simpleRuleFile "another.yaml" duplicateRuleDomain])
    )

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Assert that a gRPC request is successful and has a specific result and
-- status code.
expectSuccess
  :: (HasCallStack, Eq result, Show result)
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
  :: (HasCallStack, Show result)
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
  , serverPort      :: Port
  , serverAppState  :: AppState
  }

-- | Start Fencer on a unique port.
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
  serverPort     <- getUniquePort
  serverThreadId <- forkIO $ runServerWithPort serverPort serverLogger serverAppState

  -- NOTE(md): For reasons unkown, without a delay the delay in the thread makes a
  -- server test failure for 'test_serverResponseNoRules' go
  -- away. See <https://github.com/juspay/fencer/issues/53>.
  --
  -- The delay was introduced by assuming it might help
  -- based on the issue comment in gRPC's source code repository:
  -- https://github.com/grpc/grpc/issues/14088#issuecomment-365852100
  --
  -- The length of the delay was fine tuned based on feedback from
  -- test execution.
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

-- | Get a unique port number that is not used by other tests. Needed since
-- all tests run in parallel and we can't run Fencer on the same port in
-- parallel.
getUniquePort :: IO Port
getUniquePort = atomicModifyIORef' nextUniquePortVar (\port -> (succ port, port))

-- | Top-level global variable used by 'getUniquePort'. For the explanation
-- of @NOINLINE@, see
-- <http://neilmitchell.blogspot.com/2014/10/hlint-now-spots-bad-unsafeperformio.html>.
nextUniquePortVar :: IORef Port
nextUniquePortVar = unsafePerformIO (newIORef defaultGRPCPort)
{-# NOINLINE nextUniquePortVar #-}

----------------------------------------------------------------------------
-- gRPC client
----------------------------------------------------------------------------

-- | gRPC config that can be used to connect to Fencer started with
-- 'createServer'.
clientConfig :: Port -> Grpc.ClientConfig
clientConfig port = Grpc.ClientConfig
  { Grpc.clientServerHost = "localhost"
  , Grpc.clientServerPort = fromIntegral (unPort port)
  , Grpc.clientArgs = []
  , Grpc.clientSSLConfig = Nothing
  , Grpc.clientAuthority = Nothing
  }

-- | Create a "service" that can be used to make requests to Fencer started
-- with 'createServer'.
withService
  :: Server
  -> (Proto.RateLimitService Grpc.ClientRequest Grpc.ClientResult -> IO a)
  -> IO a
withService server act =
  Grpc.withGRPCClient (clientConfig (serverPort server)) $ \grpcClient -> do
    service <- Proto.rateLimitServiceClient grpcClient
    act service

----------------------------------------------------------------------------
-- Various useful values
----------------------------------------------------------------------------

domainDefinitionWithoutRules :: DomainDefinition
domainDefinitionWithoutRules = DomainDefinition
  { domainDefinitionId = DomainId "domain"
  , domainDefinitionDescriptors = []
  }

-- | A generic response useful for testing situations where the server
-- replies with a generic OK response.
genericOKResponse :: Proto.RateLimitResponse
genericOKResponse = Proto.RateLimitResponse
  { rateLimitResponseOverallCode =
      Enumerated $ Right Proto.RateLimitResponse_CodeOK
  , rateLimitResponseStatuses = Vector.singleton
      Proto.RateLimitResponse_DescriptorStatus
      { rateLimitResponse_DescriptorStatusCode =
          Enumerated $ Right Proto.RateLimitResponse_CodeOK
      , rateLimitResponse_DescriptorStatusCurrentLimit = Nothing
      , rateLimitResponse_DescriptorStatusLimitRemaining = 0
      }
  , rateLimitResponseHeaders = Vector.empty
  }
