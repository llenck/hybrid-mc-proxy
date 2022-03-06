module NetworkSetup (connTCP, hostTCP, withConn, withHost) where

import Control.Exception (bracket)
import Control.Monad (guard)

import Network.Socket
import Network.Socket.ByteString

notNull = not . null

makeAddr host service = do
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just service)
    guard $ notNull addrs
    return $ head addrs

makeSock addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

connTCP host service = do
    addr <- makeAddr host service
    s <- makeSock addr

    connect s $ addrAddress addr
    return s

hostTCP host service = do
    addr <- makeAddr host service
    s <- makeSock addr

    bind s $ addrAddress addr
    listen s 4
    return s

withConn host service = bracket (connTCP host service) close
withHost host service = bracket (hostTCP host service) close
