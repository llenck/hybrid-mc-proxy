import Test.Hspec

import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.VarInt.Get
import Data.VarInt.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import MCProto

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
            parsePackets (BS.pack [10, 1, 2, 3]) `shouldBe` ([], BS.pack [10, 1, 2, 3])

        it "can parse multiple packets" $ do
            let ps = [Other 0 BS.empty, Other 128 $ BS.pack [1]]
            parsePackets (BS.pack [1, 0, 3, 128, 1, 1, 69, 42]) `shouldBe` (ps, BS.pack [69, 42])

main :: IO ()
main = hspec $ do
    testVarIntGet
    testVarIntPut
    testMCProto
