-- test/Main.hs
module Main where

import           Test.Tasty (after, defaultMain, testGroup, DependencyType(AllFinish), TestTree)

import           BasePrelude

import qualified Fencer.Rules.Test
import qualified Fencer.Server.Test
import qualified Fencer.Types.Test


tests :: TestTree
tests = testGroup "All tests"
  [ Fencer.Types.Test.tests
  , Fencer.Rules.Test.tests
  -- 'after' is needed to avoid running the 'rules' and 'server' tests
  -- concurrently. Running them concurrently is problematic because
  -- both create a server (binding the same port) so if they create it
  -- at the same time, one of the test groups will fail. The 'after'
  -- function makes 'server' tests run after 'rules' tests.
  , after AllFinish "test_rules" Fencer.Server.Test.tests
  ]


main :: IO ()
main = defaultMain tests
