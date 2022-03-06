module ProxyLogic (handleClient) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (concurrently)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Network.Socket
import Network.Socket.ByteString

import qualified Data.ByteString as BS

import NetworkSetup
import qualified MCProto as P

data ProxyState = PS {
    packets :: Int
} deriving (Show)

makeState = PS 0
inc_ps st = let p' = packets st + 1 in (st {packets = p'}, p')

-- TODO this needs unconsumed stuff.
-- for testing that, just set the recv length to something small (e.g. 20)
proxyCS :: IORef ProxyState -> Socket -> Socket -> BS.ByteString -> IO ()
proxyCS st cl srv acc = do
    buf <- recv cl 65536

    -- for now, we forward everything, and just print packets.
    -- in the final thing however, we should only pass valid, parsed packets, to have a chance
    -- of doing any packet manipulation
    let (ps, rem) = P.parsePackets (acc <> buf)
    mapM (\p -> putStrLn $ "C->S :: " ++ (show p)) ps

    if BS.null buf
    then putStrLn "C->S connection closed"
    else do
        num_packets <- atomicModifyIORef' st inc_ps
        sendAll srv buf
        putStrLn $ "Passed packet #" ++ (show num_packets) ++ " (C->S)"

        proxyCS st cl srv rem

proxySC :: IORef ProxyState -> Socket -> Socket -> BS.ByteString -> IO ()
proxySC st cl srv acc = do
    buf <- recv srv 65536

    -- see comment in proxyCS
    let (ps, rem) = P.parsePackets (acc <> buf)
    mapM (\p -> putStrLn $ "S->C :: " ++ (show p)) ps

    if BS.null buf
    then putStrLn "S->C connection closed"
    else do
        num_packets <- atomicModifyIORef' st inc_ps
        sendAll cl buf
        putStrLn $ "Passed packet #" ++ (show num_packets) ++ " (S->C)"

        proxySC st cl srv rem

-- withConn ensures closing of the connection to the server
handleClient cl = withConn "69owo.de" "25566" $ \srv -> do
    st <- newIORef makeState

    -- this just runs proxyCS and proxySC until one exits, ignoring io failure
    concurrently (proxyCS st cl srv BS.empty) (proxySC st cl srv BS.empty) <|> return ((), ())
    return ()
