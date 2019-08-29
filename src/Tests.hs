module Main where

import           Test.Tasty


tests :: TestTree
tests = testGroup "Configuration Parsing" $ []


-- | Test entry point
main :: IO ()
main = defaultMain tests
