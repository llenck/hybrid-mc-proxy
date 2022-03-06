module Main where

import Control.Monad (forever)
import Control.Concurrent.Async (race)
import Network.Socket
import System.Environment (getArgs)

import NetworkSetup
import ProxyLogic

-- blocks
spawnHandlers :: Socket -> IO ()
spawnHandlers srv = forever $ accept srv >>= handleClient . fst

main = do
    s <- head <$> getArgs >>= hostTCP "0.0.0.0"
    putStrLn "Started..."
    race getLine (spawnHandlers s) -- run spawnHandlers until getLine finishes
    putStrLn "Exiting..."
    close s
