{-# LANGUAGE OverloadedStrings #-}

module BloomFilter.API
(
    BloomFilter(..),
    fromList,
    elem
)
where

import Data.Word (Word32)

import Control.Monad.ST
import Control.Monad
import Control.Applicative
import Data.Array.Unboxed

import Data.Array.ST

import Prelude hiding (elem)
import qualified Data.ByteString as BS
import qualified BloomFilter.Mutable as BFM

data BloomFilter = BloomFilter
    {
        size :: Word32,
        hashFunctions :: BFM.HashFunctions,
        array :: UArray Word32 Bool
    }


elem :: BloomFilter -> BS.ByteString -> Bool
elem (BloomFilter numBits hfs bitArr) item =
    and $ map (bitArr !) indices
    where
        hashes = map ($ item) hfs
        indices = map (`mod` numBits) hashes


fromList :: BFM.HashFunctions -> [BS.ByteString] -> Word32 -> BloomFilter
fromList hfs items numBits = BloomFilter numBits hfs $ runST $ do
    mbf <- BFM.new hfs numBits
    mapM_ (BFM.insert mbf) items
    newArr <- freeze (BFM.bitArray mbf)
    return newArr
