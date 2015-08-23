module MurmurHash
(
    murmurHash2
    )
where

import Control.Monad.ST
import Data.STRef

import Data.Bits
import Data.Word (Word32, Word8)
import qualified Data.ByteString as BS
import Data.List.Split

m = fromIntegral 0x5bd1e995
r = 24

murmurHash2LoopHelper :: Word32 -> Word32 -> Word32
murmurHash2LoopHelper h k0 = runST $ do
    hh <- newSTRef h
    let kk = k0*m
    k <- newSTRef kk
    modifySTRef k (xor (kk `shiftR` r))
    modifySTRef k (*m)
    modifySTRef hh (*m)
    a <- readSTRef k
    modifySTRef hh (xor a)
    readSTRef hh


-- returns hash value and consumed length
murmurHash2Loop :: Int -> [Word8] -> Word32 ->  (Int, [Word8], Word32)
murmurHash2Loop len ddata h
    | len >= 4 =
        let hh = murmurHash2LoopHelper h k0
        in murmurHash2Loop remainderLength tl hh
    | otherwise = (len, ddata, h)
    where (hd, tl) = splitAt 4 ddata
          k0 = bytesToWord32 hd
          remainderLength = length tl


compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

murmurHash2switch :: (Int, [Word8], Word32) -> Word32
murmurHash2switch (len, bytes, h)
    | len >= 1 = compose (take len ops) h
    | otherwise = h
    where
        case3 x = x `xor` ((fromIntegral $ last bytes) `shiftL` 16)
        case2 x = x `xor` ((fromIntegral $ bytes !! 1) `shiftL` 8)
        case1 x = x `xor` (fromIntegral $ head bytes)
        def x = x*m
        ops = [def, case1 , case2 , case3]

murmurHash2End :: Word32 -> Word32
murmurHash2End h =
    let tmp = (h `xor` (h `shiftR` 13)) * m
    in tmp `xor` (tmp `shiftR` 15)


-- dependent on endianness
bytesToWord32 :: [Word8] -> Word32
bytesToWord32 = foldl accum 0
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
