module Main where

import qualified Fencer.Types.Test as T
import           Test.Tasty


tests :: TestTree
tests = testGroup "Configuration Parsing"
  [ T.test_parseJSONDescriptorDefinition
  , T.test_parseJSONDomainDefinition ]


-- | Test entry point
main :: IO ()
main = defaultMain tests
