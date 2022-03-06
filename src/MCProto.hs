--module MCProto (Packet(..), parsePacket, parsePackets, formatPacket) where
module MCProto where

import Control.Applicative ((<|>))
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.ByteString as BS

import Data.VarInt.Get
import Data.VarInt.Put

data Packet = Foreign BS.ByteString
    | Other Int BS.ByteString deriving (Show, Eq)

--data PacketInfo = Foreign
--    | Other Int deriving (Show, Eq)

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
