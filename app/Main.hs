{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Client
import TAPI

routes = ["77", "71", "73"]

main :: IO ()
main = do
  mvars <- mapM (\r -> do
                    mvar <- newEmptyMVar
                    thread <- forkFinally (routeLoop r) (putMVar mvar)
                    return mvar)
           routes
  mapM_ (\m -> takeMVar m) mvars
  return ()

routeLoop :: TAPI.RouteID -> IO()
routeLoop r = do
  res <- queryAPI $ getVehicles r
  putStrLn $ show res
  threadDelay (15 * 1000 * 1000)
  routeLoop r
  
  
