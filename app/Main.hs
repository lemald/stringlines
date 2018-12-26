{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Database.SQLite.Simple

import Client
import DataStore
import TAPI

routes :: [RouteID]
routes = ["77", "71", "73"]

main :: IO ()
main = do
  con <- connectToDB
  createTables con
  mvars <- mapM (\r -> do
                    mvar <- newEmptyMVar
                    thread <- forkFinally (routeLoop r con) (putMVar mvar)
                    return mvar)
           routes
  mapM_ (\m -> takeMVar m) mvars
  closeDBCon con
  return ()

routeLoop :: TAPI.RouteID -> Connection -> IO()
routeLoop r con = do
  res <- queryAPI $ getVehicles r
  case res of
    (Left _) -> putStrLn "Error"
    (Right apires) ->
      let tripInfo = tripInfoFromResponse apires
      in do putStrLn (show tripInfo)
            insertTripInfo con tripInfo
            return ()
           
  threadDelay (15 * 1000 * 1000)
  routeLoop r con
  
  
