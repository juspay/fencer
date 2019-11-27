-- test/Main.hs
module Main where

import           Test.Tasty (defaultMain, testGroup, TestTree)

import           BasePrelude

import qualified Fencer.Logic.Test
import qualified Fencer.Rules.Test
import qualified Fencer.Server.Test
import qualified Fencer.Types.Test


tests :: TestTree
tests = testGroup "All tests"
  [ Fencer.Types.Test.tests
  , Fencer.Logic.Test.tests
  , Fencer.Rules.Test.tests
  , Fencer.Server.Test.tests
  ]


main :: IO ()
main = defaultMain tests
