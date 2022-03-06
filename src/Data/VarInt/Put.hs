module Data.VarInt.Put (putVarInt, formatVarInt) where

import Data.Binary.Put
import Data.Bits

import qualified Data.ByteString as BS

formatVarInt :: Int -> BS.ByteString
formatVarInt 0 = BS.empty
formatVarInt n =
    let more = shiftR n 7
        cur = fromIntegral $ n .&. 0x7f .|. (if more > 0 then 0x80 else 0)
    in cur `BS.cons` formatVarInt more

putVarInt :: Int -> Put
putVarInt = putByteString . formatVarInt
