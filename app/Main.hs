module Main where

import Control.Monad (forever)
import Control.Concurrent.Async (race)
import Network.Socket
import System.Environment (getArgs)

import NetworkSetup
import ProxyLogic

-- blocks
spawnHandlers :: Socket -> IO ()
spawnHandlers srv = forever $ withAccept srv $ handleClient . fst

main = do
    port <- head <$> getArgs
    withHost "0.0.0.0" port $ \s -> do
        putStrLn "Started..."
        race getLine (spawnHandlers s) -- run spawnHandlers until getLine finishes
        putStrLn "Exiting..."
