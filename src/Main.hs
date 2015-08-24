module Main where

import Data.Word(Word32)
import JenkinsOneAtATime
import MurmurHash
import qualified BloomFilter.API as BF
import Data.Serialize
import Data.Array.IArray

seed :: Word32
seed = 23

bitArraySize :: Word32
bitArraySize = 871

main :: IO()
main = do
    let xx = [1..100] :: [Int]
    let serializedData = map encode xx
    let hfs = [jenkinsOneAtATime, murmurHash2 seed]
    let bf = BF.fromList hfs serializedData bitArraySize
    let count = length $ filter (BF.elem bf) serializedData
    print count
