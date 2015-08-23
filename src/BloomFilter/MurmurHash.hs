module MurmurHash
(
    murmurHash2
    )
where

import Data.Bits
import Data.Word (Word32, Word8)
import qualified Data.ByteString as BS
import Data.List.Split

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
        -- ops = [def, case1 , case2 , case3]

murmurHash2End :: Word32 -> Word32
murmurHash2End h =
    let tmp = (h `xor` (h `shiftR` 13)) * m
    in tmp `xor` (tmp `shiftR` 15)


-- dependent on endianness
bytesToWord32 :: [Word8] -> Word32
bytesToWord32 x = foldl accum 0 $ reverse x
    where
        accum a o = (a `shiftL` 8) .|. fromIntegral o


murmurHash2 :: Word32 -> BS.ByteString -> Word32
murmurHash2 seed key =
    let len = BS.length key
        h = seed `xor` (fromIntegral len)
        bytes = BS.unpack key
        rr = murmurHash2Loop len bytes h
        ll = murmurHash2switch rr
    in murmurHash2End ll
