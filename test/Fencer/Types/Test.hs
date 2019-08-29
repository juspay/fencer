{-# LANGUAGE OverloadedStrings #-}

-- | Tests for types from the 'Fencer.Types' module.
module Fencer.Types.Test
  ( test_parseJSONDescriptorDefinition
  , test_parseJSONDomainDefinition
  )
where

import           Data.Aeson (parseJSON)
import           Data.Aeson.Types (parseEither, Parser, Value(..))
import           Data.Either (isLeft)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Fencer.Types (DescriptorDefinition(..), DomainDefinition(..))
import           Test.Tasty (TestTree)
import           Test.Tasty.HUnit (assertBool, testCase)


dd1 :: Value
dd1 = Object $ HM.fromList
  [ ("key", "some key")
  , ("value", "some value")
  , ("rate_limit", Null)
  , ("descriptors", Null)
  ]

rateLimit :: Value
rateLimit = Object $ HM.fromList
  [ ("unit", String "second")
  , ("requests_per_unit", Number 5)
  ]

dd2 :: Value
dd2 = Object $ HM.fromList
  [ ("key", "some key #2")
  , ("value", "some value #2")
  , ("rate_limit", rateLimit)
  , ("descriptors", Array $ V.fromList [dd1] )
  ]

o :: Value
o = Object $ HM.fromList
  [ ("domain", "some domain")
  , ("descriptors", Array $ V.fromList [ dd1, dd2 ])
  ]

type Par a = Value -> Parser a

test_parseJSONDescriptorDefinition :: TestTree
test_parseJSONDescriptorDefinition =
  testCase "Successful JSON parsing of DescriptorDefinition" $ do
    assertBool "parsing DescriptorDefinition failed" $ not . isLeft $
      parseEither (parseJSON :: Par DescriptorDefinition) dd1

test_parseJSONDomainDefinition :: TestTree
test_parseJSONDomainDefinition =
  testCase "Successful JSON parsing of DomainDefinition" $ do
    assertBool "parsing DomainDefinition failed" $ not . isLeft $
      parseEither (parseJSON :: Par DomainDefinition) o
