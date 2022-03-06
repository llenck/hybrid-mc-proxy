module MCCrypto (go) where

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

import OpenSSL.EVP.Cipher

key_example = B8.pack "01234567890123456789012345678901"
iv_example = B8.pack "0123456789012345"
ciph = let (Just c) = unsafePerformIO $ getCipherByName "aes-256-cfb8" in c

decStream :: BS.ByteString -> BS.ByteString -> BL.ByteString -> IO BL.ByteString
decStream key iv s = cipherLBS ciph key iv Decrypt s
encStream :: BS.ByteString -> BS.ByteString -> BL.ByteString -> IO BL.ByteString
encStream key iv s = cipherLBS ciph key iv Encrypt s

go :: IO ()
go = do
    return ()
