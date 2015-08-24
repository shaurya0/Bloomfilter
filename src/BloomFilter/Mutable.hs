{-# LANGUAGE OverloadedStrings #-}

module BloomFilter.Mutable
(
    MutableBloomFilter(bitArray, hashFunctions),
    new,
    insert,
    elem,
    HashFunctions,
)
where

import Data.Word (Word8, Word32)

import Control.Monad.ST
import Control.Monad
import Control.Applicative
import Data.Array.ST
import Prelude hiding (elem)
import qualified Data.ByteString as BS


type HashFunction = (BS.ByteString -> Word32)
type HashFunctions = [HashFunction]
data MutableBloomFilter s = MutableBloomFilter
    {
        hashFunctions :: HashFunctions,
        size :: Word32,
        bitArray :: STUArray s Word32 Bool
    }


new :: HashFunctions -> Word32 -> ST s (MutableBloomFilter s)
new hfs numBits = MutableBloomFilter hfs numBits `liftM` newArray (0, numBits - 1) False

insert :: MutableBloomFilter s -> BS.ByteString -> ST s ()
insert bf@(MutableBloomFilter hfs numBits arr) item =
    mapM_ (\i -> writeArray arr i True) indices
    where
        hashes = map ($ item) hfs
        indices = map (`mod` numBits) hashes

elem :: MutableBloomFilter s -> BS.ByteString -> ST s Bool
elem (MutableBloomFilter hfs numBits arr) item = do
        result <- mapM (readArray arr) indices
        return $ and result
    where
        hashes = map ($ item) hfs
        indices = map (`mod` numBits) hashes
