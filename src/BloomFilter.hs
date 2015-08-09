{-# LANGUAGE OverloadedStrings #-}

module BloomFilter where

import Data.Bits
import Data.BitArray
import Data.Word
import Data.List.Split
import qualified Data.ByteString as BS


data BloomFilter = BloomFilter
    {
        size :: Word32,
        bitVector :: BitArray,
        hashFunctions :: [(Word32 -> Word32)]
    }

insert :: BloomFilter -> BS.ByteString -> BloomFilter
insert bf item = error "todo"

lookup :: BloomFilter -> BS.ByteString -> Bool
lookup bf item = error "todo"

testStr :: BS.ByteString
testStr = "somethingsomething"

innerLoopHelper :: Word32 -> Word32 -> Word32
innerLoopHelper x y =
    let s1 = x+y
        s2 = s1 + (s1 `shiftL` 10)
    in s2 `xor` (s2 `shiftR` 6)

resultHelper :: Word32 -> Word32
resultHelper x =
    let s1 = x + (x `shiftL` 3)
        s2 = s1 `xor` (s1 `shiftR` 11)
    in s2 + (s2 `shiftL` 15)

jenkinsOneAtATime :: BS.ByteString -> Word32
jenkinsOneAtATime key =
    let hash = foldl innerLoopHelper 0 $ map fromIntegral $ BS.unpack key
    in  resultHelper hash

m = fromIntegral 0x5bd1e995
r = 24

-- returns hash value and consumed length
murmurHash2Loop :: Int -> [Word8] -> Word32 ->  (Int, [Word8], Word32)
murmurHash2Loop len ddata h
    | len >= 4 =
        let k1 = k0 * m
            k2 = k1 `xor` (k1 `shiftR` r)
            k3 = k2 * m
            h1 = h * m
            h2 = h1 `xor` k3
        in murmurHash2Loop remainderLength tl h2
    | otherwise = (len, ddata, h)
        where (hd, tl) = splitAt 4 ddata
              k0 = bytesToWord32 hd
              remainderLength = length tl

murmurHash2switch :: (Int, [Word8], Word32) -> Word32
murmurHash2switch (len, bytes, h)
    | len == 3 = (def . case1 . case2 . case3) h
    | len == 2 = (def . case1 . case2) h
    | len == 1 = (def . case1) h
    | otherwise = def h
    where
        case3 x = x `xor` ((fromIntegral $ last bytes) `shiftL` 16)
        case2 x = x `xor` ((fromIntegral $ bytes !! 1) `shiftL` 8)
        case1 x = x `xor` (fromIntegral $ head bytes)
        def x = x*m

murmurHash2End :: Word32 -> Word32
murmurHash2End h =
    let tmp = (h `xor` (h `shiftR` 13)) * m
    in tmp `xor` (tmp `shiftR` 15)


-- dependent on endianness
bytesToWord32 :: [Word8] -> Word32
bytesToWord32 x = foldl accum 0 $ reverse x
    where
        accum a o = (a `shiftL` 8) .|. fromIntegral o


testArr :: [Word8]
testArr = [1,2,3,4]

murmurHash2 :: BS.ByteString -> Word32 -> Word32
murmurHash2 key seed =
    let len = BS.length key
        h = seed `xor` (fromIntegral len)
        bytes = BS.unpack key
        rr = murmurHash2Loop len bytes h
        ll = murmurHash2switch rr
    in murmurHash2End ll
