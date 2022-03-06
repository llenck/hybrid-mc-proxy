module Data.VarInt.Get (parseVarInt, parseVarIntLen, parseVarLong, parseVarLongLen) where

import Data.Binary.Strict.Get
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.ByteString as BS

-- (varint, consumed_bytes)
-- can't implement this in terms of "bytesRead", because that also counts bytes read
-- by other things if our caller is also a "Get" instance
parseVarWord64Len :: Get (Word64, Int)
parseVarWord64Len = do
    b <- fromIntegral <$> getWord8
    case (b .&. 0x80) of
        0x80 -> do
            (rem_int, rem_bytes) <- parseVarWord64Len
            return (b .&. 0x7F .|. shiftL rem_int 7, 1 + rem_bytes)
        0 -> return (b .&. 0x7F, 1)

parseVarIntLen :: Get (Int, Int)
parseVarIntLen = do
    (i, n) <- parseVarWord64Len
    let i_signed = fromIntegral i :: Int32
    return (fromIntegral i_signed, n)

parseVarLongLen :: Get (Int, Int)
parseVarLongLen = do
    (i, n) <- parseVarWord64Len
    let i_signed = fromIntegral i :: Int64
    return (fromIntegral i_signed, n)

parseVarInt = fst <$> parseVarIntLen
parseVarLong = fst <$> parseVarLongLen
