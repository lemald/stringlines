module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Client

main :: IO ()
main = do
  -- We'll be getting an MVar for each thread to wait on it
  mvar <- newEmptyMVar
  thread <- forkFinally (threadDelay 5000000 >> putStrLn "foo") (putMVar mvar)
  takeMVar mvar
  return ()
