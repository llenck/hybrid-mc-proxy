--module MCProto (Packet(..), parsePacket, parsePackets, formatPacket) where
module MCProto where

import Control.Applicative ((<|>))
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Bits

import qualified Data.ByteString as BS

data Packet = Foreign BS.ByteString
    | Other Int BS.ByteString deriving (Show, Eq)

--data PacketInfo = Foreign
--    | Other Int deriving (Show, Eq)

--parseVarInt = fst <$> parseVarIntLen

parseVarInt :: Get Int
parseVarInt = do
    b <- fromIntegral <$> getWord8
    case (b .&. 0x80) of
        0x80 -> (\rem_int -> b .&. 0x7F .|. shiftL rem_int 7) <$> parseVarInt
        0 -> return $ b .&. 0x7F

-- (varint, consumed_bytes)
-- can't implement this in terms of "bytesRead", because that also counts bytes read
-- by other things if our caller is also a "Get" instance
parseVarIntLen :: Get (Int, Int)
parseVarIntLen = do
    b <- fromIntegral <$> getWord8
    case (b .&. 0x80) of
        0x80 -> do
            (rem_int, rem_bytes) <- parseVarIntLen
            return (b .&. 0x7F .|. shiftL rem_int 7, 1 + rem_bytes)
        0 -> return (b .&. 0x7F, 1)


formatVarInt :: Int -> BS.ByteString
formatVarInt 0 = BS.empty
formatVarInt n =
    let more = shiftR n 7
        cur = fromIntegral $ n .&. 0x7f .|. (if more > 0 then 0x80 else 0)
    in cur `BS.cons` formatVarInt more

putVarInt n = putByteString $ formatVarInt n

parseValidPacket :: Get Packet
parseValidPacket = do
    len <- parseVarInt
    (p_id, id_len) <- parseVarIntLen
    case p_id of
        _ -> Other p_id <$> getByteString (len - id_len)

formatPacket :: Packet -> Put
formatPacket (Foreign bs) = putByteString bs   -- packet content
formatPacket (Other p_id bs) = do
    let id_bs = formatVarInt p_id
    putVarInt $ BS.length bs + BS.length id_bs -- length of the packet
    putByteString id_bs                        -- packet id
    putByteString bs                           -- packet content

-- either parse a minecraft packet, or return "Foreign" if we don't understand it
parsePacket = parseValidPacket <|> Foreign <$> (remaining >>= getByteString)

-- TODO this doesn't work, we aren't guaranteed to receive full packets.
-- we need to return unconsumed bytes and our caller needs more state.
parsePackets_ :: [Packet] -> BS.ByteString -> [Packet]
parsePackets_ acc bs = if BS.null bs
    then acc
    else let (Right res, more) = runGet parsePacket bs in
        parsePackets_ (acc ++ [res]) more

parsePackets = parsePackets_ []
