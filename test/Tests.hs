module Main where

import           Fencer.Types.Test (test_parseJSONDomainDefinition, test_parseJSONDescriptorDefinition)
import           Test.Tasty


tests :: TestTree
tests = testGroup "Configuration Parsing"
  [ test_parseJSONDescriptorDefinition
  , test_parseJSONDomainDefinition ]


-- | Test entry point
main :: IO ()
main = defaultMain tests
