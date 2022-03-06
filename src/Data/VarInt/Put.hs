module Data.VarInt.Put (formatVarInt, formatVarLong, putVarInt, putVarLong) where

import Data.Binary.Put
import Data.Bits
import Data.Word

import qualified Data.ByteString as BS

formatVarWord64_ :: Word64 -> BS.ByteString
formatVarWord64_ 0 = BS.empty
formatVarWord64_ n =
    let more = shiftR n 7
        cur = fromIntegral $ n .&. 0x7f .|. (if more > 0 then 0x80 else 0)
    in cur `BS.cons` formatVarWord64_ more

formatVarWord64 :: Word64 -> BS.ByteString
formatVarWord64 0 = BS.singleton 0
formatVarWord64 n = formatVarWord64_ n

formatVarLong :: Int -> BS.ByteString
formatVarLong = formatVarWord64 . fromIntegral
formatVarInt :: Int -> BS.ByteString
formatVarInt n = formatVarWord64 $ fromIntegral (fromIntegral n :: Word32)

putVarInt :: Int -> Put
putVarInt = putByteString . formatVarInt
putVarLong :: Int -> Put
putVarLong = putByteString . formatVarLong
