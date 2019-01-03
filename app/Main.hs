{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as Ex
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
                    thread <- forkFinally
                      (initiateRouteLoop r con)
                      (putMVar mvar)
                    return mvar)
           routes
  mapM_ (\m -> takeMVar m) mvars
  putStrLn "All child threads exited, shutting down."
  closeDBCon con
  return ()

initiateRouteLoop :: TAPI.RouteID -> Connection -> IO()
initiateRouteLoop r con = do
  shapeResponse <- queryAPI $ getShapes r
  case shapeResponse of
    (Left err) -> putStrLn("Error fetching shape data from API: "
                           ++ show err)
    (Right apires) -> routeLoop r con

routeLoop :: TAPI.RouteID -> Connection -> IO()
routeLoop r con = do
  res <- queryAPI $ getVehicles r
  case res of
    (Left err) -> putStrLn ("Error fetching vehicle data from API: "
                            ++ show err)
    (Right apires) -> do
      sql_res <- Ex.try (let tripInfo = tripInfoFromResponse apires
                         in do putStrLn ("Fetched "
                                         ++ show (length tripInfo)
                                         ++ " locations for route "
                                         ++ show r
                                         ++ ".")
                               insertTripInfo con tripInfo)
      case sql_res of
        Left e -> putStrLn ("Exception raised: "
                            ++ show (e :: Ex.SomeException))
        Right _ -> return()
           
  threadDelay (15 * 1000 * 1000)
  routeLoop r con
  
  
