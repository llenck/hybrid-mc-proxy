{-# LANGUAGE CApiFFI #-}

module MCCrypto (go) where

import Control.Monad (guard)
import Data.Void (Void)
import Foreign
import Foreign.C
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BI

newtype EvpCtx = EvpCtx Void
newtype EvpCipher = EvpCipher Void

-- convention: foreign unsafe stuff has suffix of '__'
foreign import capi "openssl/evp.h EVP_CIPHER_CTX_new" evpCreate__ :: IO (Ptr EvpCtx)
foreign import capi "openssl/evp.h &EVP_CIPHER_CTX_free" evpDestroy__ :: FinalizerPtr EvpCtx
foreign import capi "openssl/evp.h EVP_aes_256_cfb8" evpAes256Cfb8__ :: IO (Ptr EvpCipher)
foreign import capi "openssl/evp.h EVP_CipherInit" cipherInit__ :: Ptr EvpCtx -> Ptr EvpCipher -> Ptr CUChar -> Ptr CUChar -> CInt -> IO CInt

foreign import capi "openssl/evp.h EVP_CIPHER_key_length" cipherKeyLength__ :: Ptr EvpCipher -> CInt
foreign import capi "openssl/evp.h EVP_CIPHER_iv_length"  cipherIvLength__  :: Ptr EvpCipher -> CInt

keyLength :: Int
keyLength = fromIntegral $ unsafePerformIO $ cipherKeyLength__ <$> evpAes256Cfb8__
ivLength :: Int
ivLength  = fromIntegral $ unsafePerformIO $  cipherIvLength__ <$> evpAes256Cfb8__

evpCreate = evpCreate__ >>= newForeignPtr evpDestroy__

aesCfb8Create :: Bool -> BS.ByteString -> BS.ByteString -> IO (ForeignPtr EvpCtx)
aesCfb8Create enc key iv = do
    guard $ BS.length key == keyLength
    guard $ BS.length iv == ivLength

    evp <- evpCreate
    ciph <- evpAes256Cfb8__
    let (key_p, _, _) = BI.toForeignPtr key
    let (iv_p, _, _) = BI.toForeignPtr iv

    withForeignPtr evp $ \ep ->
        withForeignPtr key_p $ \kp ->
            withForeignPtr iv_p $ \ ip ->
                cipherInit__ ep ciph (castPtr kp) (castPtr ip) (if enc then 1 else 0)
    return evp

createDec = aesCfb8Create False
createEnc = aesCfb8Create True

go = do
    cipherKeyLength__ <$> evpAes256Cfb8__ >>= print
    --a <- createDec
    return ()
