module ProxyLogic (handleClient) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (concurrently)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as BS

import NetworkSetup
import MCProto

data ProxyState = PS {
    packets :: Int
} deriving (Show)

makeState = PS 0
inc_ps st = let p' = packets st + 1 in (st {packets = p'}, p')

proxy_ :: BS.ByteString -> ([Packet] -> IO ()) -> Socket -> IO ()
proxy_ acc cb s = do
    buf <- recv s 65536
    if BS.null buf
    then cb []
    else do
        let (ps, rem) = case parsePackets (acc <> buf) of
                Right x -> x
                Left err -> error err
        cb ps
        proxy_ rem cb s

proxy :: ([Packet] -> IO ()) -> Socket -> IO ()
proxy = proxy_ BS.empty

passPacket :: Socket -> Packet -> IO ()
passPacket s pk = sendAll s $ formatPacket pk

passPackets :: Socket -> [Packet] -> IO ()
passPackets s ps = sendAll s $ mconcat $ map formatPacket ps

proxyLog :: IORef ProxyState -> String -> Socket -> Socket -> IO ()
proxyLog st dir from to = flip proxy from $ \ps -> do
    if null ps
    then putStrLn $ dir ++ " connection closed"
    else do
        mapM (\p -> putStrLn $ dir ++ " :: " ++ (show p)) ps
        num_packets <- atomicModifyIORef' st inc_ps
        passPackets to ps
        putStrLn $ "Passed packet #" ++ (show num_packets) ++ " (" ++ dir ++ ")"

-- withConn ensures closing of the connection to the server
handleClient cl = withConn "69owo.de" "25566" $ \srv -> do
    st <- newIORef makeState

    -- this just runs proxyCS and proxySC until one exits, ignoring io failure
    let mkProxy = proxyLog st
    concurrently (mkProxy "C->S" cl srv) (mkProxy "S->C" srv cl) <|> return ((), ())
    return ()
