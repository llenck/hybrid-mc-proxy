import Test.Hspec

import Data.Binary.Put
import Data.Binary.Strict.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.Socket
import Network.Socket.ByteString
import System.Random (randomIO)

import Data.VarInt.Get
import Data.VarInt.Put
import MCProto
import NetworkSetup

testVarIntGet = do
    describe "Data.VarInt.Get" $ parallel $ do
        it "can parse non-negative VarInts" $ do
           runGet parseVarIntLen (BS.pack [0]) `shouldBe` (Right (0, 1), BS.empty)
           runGet parseVarIntLen (BS.pack [69]) `shouldBe` (Right (69, 1), BS.empty)
           runGet parseVarIntLen (BS.pack [128, 1]) `shouldBe` (Right (128, 2), BS.empty)
           runGet parseVarIntLen (BS.pack [69, 1, 2]) `shouldBe` (Right (69, 1), BS.pack [1, 2])

        it "can parse negative VarInts" $ do
            runGet parseVarInt (BS.pack [255, 255, 255, 255, 15]) `shouldBe` (Right (-1), BS.empty)

            let bs = BS.pack [128, 128, 128, 128, 8]
            runGet parseVarInt bs `shouldBe` (Right (-2147483648), BS.empty)

        it "can parse non-negative VarLongs" $ do
            runGet parseVarLongLen (BS.pack [128, 1]) `shouldBe` (Right (128, 2), BS.empty)

            let bs = BS.pack $ take 8 (repeat 255) ++ [0x7F]
            runGet parseVarLong bs `shouldBe` (Right (9223372036854775807), BS.empty)

        it "can parse negative VarLongs" $ do
            let bs = BS.pack $ take 9 (repeat 255) ++ [1, 69]
            runGet parseVarLong bs `shouldBe` (Right (-1), BS.pack [69])

testVarIntPut = do
    describe "Data.VarInt.Put" $ parallel $ do
        it "can format non-negative VarInts" $ do
            runPut (putVarInt 0) `shouldBe` BL.pack [0]
            runPut (putVarInt 69) `shouldBe` BL.pack [69]
            runPut (putVarInt 128) `shouldBe` BL.pack [128, 1]

        it "can format negative VarInts" $ do
            runPut (putVarInt (-1)) `shouldBe` BL.pack [255, 255, 255, 255, 15]
            runPut (putVarInt (-2147483648)) `shouldBe` BL.pack [128, 128, 128, 128, 8]

        it "can format non-negative VarLongs" $ do
            runPut (putVarLong 128) `shouldBe` BL.pack [128, 1]

            let bs = BL.pack $ take 8 (repeat 255) ++ [0x7F]
            runPut (putVarLong 9223372036854775807) `shouldBe` bs

        it "can format negative VarLongs" $ do
            let bs = BL.pack $ take 9 (repeat 255) ++ [1]
            runPut (putVarLong (-1)) `shouldBe` bs

testMCProto = do
    describe "MCProto" $ do
        it "can handle unfinished packets" $ do
            parsePackets (BS.pack [10, 1, 2]) `shouldBe` Right ([], BS.pack [10, 1, 2])

        it "can parse multiple packets" $ do
            let ps = [Other 0 BS.empty, Other 128 $ BS.pack [1]]
            parsePackets (BS.pack [1, 0, 3, 128, 1, 1, 69, 42]) `shouldBe` Right (ps, BS.pack [69, 42])

        it "can reject packets with abnormal sizes" $ do
            let putVi = BL.toStrict . runPut . putVarInt
            parsePackets (putVi (2^30)) `shouldBe` Left "packet too large"
            parsePackets (putVi (-10)) `shouldBe` Left "packet with negative length"

        it "can format previously parsed packets" $ do
            let bs = BS.pack [1, 0, 5, 128, 1, 69, 4, 20]
            let (Right (ps, _)) = parsePackets bs
            mconcat (map formatPacket ps) `shouldBe` bs

-- NOTE: this *doesn't* check whether socket cleanup has been performed, just that
-- the sockets work during their lifetime
testNetwork = do
    let randomPort = do
            port_num <- randomIO :: IO Int
            return (show $ (port_num `mod` 50000) + 10000)

    describe "NetworkSetup" $ context "(can fail randomly, if a random port is already bound)" $ do
        let bs = BS.pack [69, 42, 0]
        let manualSockets = do
                port <- randomPort
                srv <- hostTCP "127.0.0.1" port
                cl <- connTCP "127.0.0.1" port
                (s_con, _) <- accept srv

                sendAll cl bs
                res <- recv s_con 3

                mapM_ close [s_con, srv, cl]
                return res
        before manualSockets $ do
            it "can create listening / connected sockets manually" $ \res -> do
                res `shouldBe` bs

        let withSockets = do
                port <- randomPort
                withHost "127.0.0.1" port $ \srv -> do
                    withConn "127.0.0.1" port $ \cl -> do
                        withAccept srv $ \(s_con, _) -> do
                            sendAll cl bs
                            res <- recv s_con 3
                            return res

        before withSockets $ do
            it "can create / close listening / connected sockets automatically" $ \res -> do
                res `shouldBe` bs

main :: IO ()
main = hspec $ do
    testVarIntGet
    testVarIntPut
    testMCProto
    testNetwork
