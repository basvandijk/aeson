-- This module is only for testing deprecated features, so we can
-- silence compiler warnings selectively.

{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Properties.Deprecated (deprecatedTests) where

import Data.Aeson.Types
import Data.Aeson
import Data.Aeson.Parser (value)
import qualified Data.Attoparsec.Lazy as L
import Data.Data (Data)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Aeson.Generic as G
import qualified Data.Map as Map
import Types (Foo(..), UFoo(..))
import Instances ()


genericTo :: (Data a, ToJSON a) => a -> a -> Bool
genericTo _ v = G.encode v == encode v

genericFrom :: (Eq a, Data a, ToJSON a) => a -> a -> Bool
genericFrom _ x =
    case fmap G.fromJSON . L.parse value . encode $ x of
      L.Done _ (Success x') -> x == x'
      _ -> False

genericToFromJSON :: (Arbitrary a, Eq a, Data a) => a -> Bool
genericToFromJSON x =
    case fmap G.fromJSON . L.parse value . G.encode $ x of
      L.Done _ (Success x') -> x == x'
      _ -> False

regress_gh72 :: [(String, Maybe String)] -> Bool
regress_gh72 ys = G.decode (G.encode m) == Just m
    where m = Map.fromList ys

deprecatedTests :: [Test]
deprecatedTests = [
  testGroup "regression" [
    testProperty "gh-72" regress_gh72
  ],
  testGroup "genericFrom" [
      testProperty "Bool" $ genericFrom True
    , testProperty "Double" $ genericFrom (1::Double)
    , testProperty "Int" $ genericFrom (1::Int)
    , testProperty "Foo" $ genericFrom (undefined::Foo)
    , testProperty "Maybe" $ genericFrom (Just 1 :: Maybe Int)
    ],
  testGroup "genericTo" [
      testProperty "Bool" $ genericTo True
    , testProperty "Double" $ genericTo (1::Double)
    , testProperty "Int" $ genericTo (1::Int)
    , testProperty "Foo" $ genericTo (undefined::Foo)
    , testProperty "Maybe" $ genericTo (Just 1 :: Maybe Int)
    ],
  testGroup "genericToFromJSON" [
      testProperty "_UFoo" (genericToFromJSON :: UFoo -> Bool)
    ]
  ]
