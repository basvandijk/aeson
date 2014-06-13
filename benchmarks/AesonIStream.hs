{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson

import qualified Data.Text as Text
import Data.Text (Text)

import GHC.Generics

import Criterion.Main
import Criterion.Config
import Criterion.Monad

import Control.DeepSeq (NFData)
import Control.Applicative (ZipList(ZipList), getZipList, (<*>), (<$>))

config :: Config
config = defaultConfig { cfgConfInterval = ljust 0.95 }

crit :: Criterion ()
crit = return ()

roundTrip :: (FromJSON a, ToJSON a) => [a] -> Maybe a
roundTrip = decode . encode

len :: Int
len = 100000

benchmark1 :: (NFData b) => ([Int] -> b) -> Int -> Pure
benchmark1 f n =
  let xs :: [Int]
      xs = take n [0..]
  in nf f xs


benchmark2 :: (NFData b) => ([(Int, Int)] -> b) -> Int -> Pure
benchmark2 f n =
  let xs :: [Int]
      xs = take n [0..]
  in nf f (zip xs xs)

benchmark3 :: (NFData b) => ([Either String Integer] -> b) -> Int -> Pure
benchmark3 f n =
 let ls, rs :: [Either String Integer]
     ls = replicate n (Left "hello world!")
     x = 2^(1000 :: Integer) - 1
     rs = replicate n (Right x)
  in nf f (concat $ zipWith (\l r -> [l, r]) ls rs)


data BigRecord =
  BigRecord {
    double :: Double,
    text :: Text,
    string :: StrRecord } deriving (Show, Generic)

instance FromJSON BigRecord
instance ToJSON BigRecord
instance NFData BigRecord


data StrRecord =
  StrRecord {
    string0 :: String,
    string1 :: String,
    string2 :: String } deriving (Show, Generic)

instance FromJSON StrRecord
instance ToJSON StrRecord
instance NFData StrRecord


benchmark4 :: (NFData b) => ([BigRecord] -> b) -> Int -> Pure
benchmark4 f n =
  let str = replicate 100 'a'

      zlist = ZipList (repeat str)
      zs = StrRecord <$> zlist <*> zlist <*> zlist

      num = ZipList (take n [0..])

      txt = Text.pack str
      recs = getZipList
             $ BigRecord <$> num <*> (ZipList $ repeat txt) <*> zs

  in nf f recs


roundTripBM :: Benchmark
roundTripBM =
  bgroup "roundTrip benchmark" $
    (bench ("list of " ++ show len)
           $ benchmark1 roundTrip len) :
    (bench ("list of " ++ show (len `div` 2) ++ " pairs")
           $ benchmark2 roundTrip (len `div` 2)) :
    (bench ("list of " ++ show (len `div` 100) ++ " Either String Integer")
           $ benchmark3 roundTrip (len `div` 100)) :
    (bench ("list of " ++ show (len `div` 10) ++ " BigRecord")
           $ benchmark4 roundTrip (len `div` 10)) :
    []


encodeBM :: Benchmark
encodeBM =
  bgroup "encode benchmark" $
    (bench ("list of " ++ show len)
           $ benchmark1 encode len) :
    (bench ("list of " ++ show (len `div` 2) ++ " pairs")
           $ benchmark2 encode (len `div` 2)) :
    (bench ("list of " ++ show (len `div` 100) ++ " Either String Integer")
           $ benchmark3 encode (len `div` 100)) :
    (bench ("list of " ++ show (len `div` 10) ++ " BigRecord")
           $ benchmark4 encode (len `div` 10)) :
    []

main :: IO ()
main =
  defaultMainWith config crit $
    encodeBM :
    roundTripBM :
    []

