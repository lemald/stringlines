{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Client
import TAPI

routes :: [RouteID]
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
  let output = case res of
        (Left _) -> "Error"
        (Right apires) -> show $ tripInfoFromResponse apires
    in putStrLn output
  threadDelay (15 * 1000 * 1000)
  routeLoop r
  
  
