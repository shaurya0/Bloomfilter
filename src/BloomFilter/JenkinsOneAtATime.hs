module JenkinsOneAtATime
(
    jenkinsOneAtATime,
)
where

import Data.Bits
import Data.Word
import qualified Data.ByteString as BS

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
    let hash = foldl innerLoopHelper 0 $ map fromIntegral $ (BS.unpack . BS.reverse) key
    in  resultHelper hash
