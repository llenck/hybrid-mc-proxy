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

-- TODO this need unconsumed stuff.
-- for testing that, just set the recv length to something small (e.g. 20)
proxyCS :: IORef ProxyState -> Socket -> Socket -> IO ()
proxyCS st cl srv = do
    buf <- recv cl 65536


    -- TODO this is just for debugging
    let ps = P.parsePackets buf
    mapM (\p -> putStrLn $ "C->S :: " ++ (show p)) ps


    if BS.null buf
    then putStrLn "C->S connection closed"
    else do
        num_packets <- atomicModifyIORef' st inc_ps
        sendAll srv buf
        putStrLn $ "Passed packet #" ++ (show num_packets) ++ " (C->S)"

        proxyCS st cl srv

proxySC :: IORef ProxyState -> Socket -> Socket -> IO ()
proxySC st cl srv = do
    buf <- recv srv 65536
    if BS.null buf
    then putStrLn "S->C connection closed"
    else do
        num_packets <- atomicModifyIORef' st inc_ps
        sendAll cl buf
        putStrLn $ "Passed packet #" ++ (show num_packets) ++ " (S->C)"

        proxySC st cl srv

handleClient cl = do
    --srv <- connTCP "69owo.de" "25565"
    srv <- connTCP "69owo.de" "25566"
    state <- newIORef makeState

    -- this just runs proxyCS and proxySC until one exits, and ignores exceptions
    -- thrown by either of them
    concurrently (proxyCS state cl srv) (proxySC state cl srv) <|> return ((), ())
    return ()
