module Main where

import           Test.Tasty (after, defaultMain, testGroup, DependencyType(AllFinish), TestTree)

import           BasePrelude

import qualified Fencer.Rules.Test as R
import qualified Fencer.Server.Test as S
import qualified Fencer.Types.Test as T


tests :: TestTree
tests = testGroup "All tests"
  [ types
  , rules
  -- 'after' is needed to avoid running the 'rules' and 'server' tests
  -- concurrently. Running them concurrently is problematic because
  -- both create a server (binding the same port) so if they create it
  -- at the same time, one of the test groups will fail. The 'after'
  -- function makes 'server' tests run after 'rules' tests.
  , after AllFinish "test_rules" server
  ]

server :: TestTree
server = testGroup "Server tests" [S.test_serverResponseNoRules]

rules :: TestTree
rules = testGroup "Rule tests"
  [ R.test_rulesLoadRulesYaml
  , R.test_rulesLoadRulesNonYaml
  , R.test_rulesLoadRulesRecursively
  , R.test_rulesLimitUnitChange
  ]

types :: TestTree
types = testGroup "Type tests"
  [ T.test_parseJSONDescriptorDefinition
  , T.test_parseJSONDomainDefinition
  , T.test_parseJSONDomainAtLeastOneDescriptor
  , T.test_parseJSONNonEmptyDomainId
  , T.test_parseJSONOptionalDescriptorFields
  ]


main :: IO ()
main = defaultMain tests
