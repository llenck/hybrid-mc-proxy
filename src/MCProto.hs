module MCProto (Packet(..), parsePackets, formatPacket) where

import Control.Applicative ((<|>), empty)
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Bits
import Data.Int
import Data.Word

import qualified Data.ByteString as BS

import Data.VarInt.Get
import Data.VarInt.Put

data Packet = Other Int BS.ByteString deriving (Show, Eq)

--data PacketInfo = Other Int deriving (Show, Eq)

getPacket :: Get Packet
getPacket = do
    len <- parseVarInt
    (p_id, id_len) <- parseVarIntLen
    case p_id of
        _ -> Other p_id <$> getByteString (len - id_len)

-- consume input only if we actually get a full packet
nongreedyGetPacket = do
    may_pk <- lookAheadM $ (Just <$> getPacket) <|> pure Nothing
    case may_pk of
        Just pk -> return pk
        Nothing -> empty -- fail

formatPacket :: Packet -> Put
formatPacket (Other p_id bs) = do
    let id_bs = formatVarInt p_id
    putVarInt $ BS.length bs + BS.length id_bs -- length of the packet
    putByteString id_bs                        -- packet id
    putByteString bs                           -- packet content

parsePackets_ :: [Packet] -> BS.ByteString -> ([Packet], BS.ByteString)
parsePackets_ acc bs =
    let (may_pk, rem) = runGet nongreedyGetPacket bs in
        case may_pk of
            Left _ -> (acc, rem)
            Right pk -> parsePackets_ (acc ++ [pk]) rem

parsePackets :: BS.ByteString -> ([Packet], BS.ByteString)
parsePackets = parsePackets_ []
