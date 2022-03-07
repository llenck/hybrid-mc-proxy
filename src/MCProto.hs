module MCProto (Packet(..), parsePackets, formatPacket) where

import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.VarInt.Get
import Data.VarInt.Put

data Packet = Other Int BS.ByteString deriving (Show, Eq)

-- a later plan might be to cache the non-parsed version of the packet, so that if we
-- just pass it, we don't have to format it. Then we'd have something like
-- "data Packet = Packet BS.ByteString PacketInfo"

--data PacketInfo = Other Int deriving (Show, Eq)

failIf msg b = if b then fail msg else pure ()

getPacket :: Get Packet
getPacket = do
    len <- parseVarInt
    failIf "packet too large" $ len > 2^22 -- 4 MiB
    failIf "packet with negative length" $ len < 0
    (p_id, id_len) <- parseVarIntLen
    case p_id of
        -- TODO to parse different kinds of packets, we need information on the
        -- current state of the protocol :(
        _ -> Other p_id <$> getByteString (len - id_len)

formatPacket_ :: Packet -> Put
formatPacket_ (Other p_id bs) = do
    let id_bs = formatVarInt p_id
    putVarInt $ BS.length bs + BS.length id_bs -- length of the packet
    putByteString id_bs                        -- packet id
    putByteString bs                           -- packet content

formatPacket :: Packet -> BS.ByteString
formatPacket = BL.toStrict . runPut . formatPacket_

parsePackets_ :: [Packet] -> BS.ByteString -> Either String ([Packet], BS.ByteString)
parsePackets_ acc bs =
    let (may_pk, rem) = runGet getPacket bs in
        case may_pk of
            Right pk -> parsePackets_ (acc ++ [pk]) rem
            -- getPacket may have eaten some input, so return "bs" instead of "rem"
            Left "too few bytes" -> Right (acc, bs)
            Left err -> Left err

parsePackets :: BS.ByteString -> Either String ([Packet], BS.ByteString)
parsePackets = parsePackets_ []
