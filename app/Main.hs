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

data RouteConf = RouteConf RouteID ShapeID

routeConfRoute :: RouteConf -> RouteID
routeConfRoute (RouteConf r _) = r

routeConfShapeID :: RouteConf -> ShapeID
routeConfShapeID (RouteConf _ s) = s

routes :: [RouteConf]
routes = [
  RouteConf "77" "770105"
  ]

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

initiateRouteLoop :: RouteConf -> Connection -> IO()
initiateRouteLoop rc con = do
  shapeResponse <- queryAPI $ getShapes (routeConfRoute rc)
  case shapeResponse of
    (Left err) -> putStrLn("Error fetching shape data from API: "
                           ++ show err)
    (Right apires) -> let shapeEntities = entitiesFromResponse apires
                      in routeLoop rc shapeEntities con

routeLoop :: RouteConf -> [Entity Shape] -> Connection -> IO()
routeLoop rc shapeEntities con = do
  res <- queryAPI $ getVehicles (routeConfRoute rc)
  case res of
    (Left err) -> putStrLn ("Error fetching vehicle data from API: "
                            ++ show err)
    (Right apires) -> do
      sql_res <- Ex.try (let s = attributesByID shapeEntities
                                 $ routeConfShapeID rc
                             tripInfo = tripInfoFromResponse apires s
                         in do putStrLn ("Fetched "
                                         ++ show (length tripInfo)
                                         ++ " locations for route "
                                         ++ show (routeConfRoute rc)
                                         ++ ".")
                               insertTripInfo con tripInfo)
      case sql_res of
        Left e -> putStrLn ("Exception raised: "
                            ++ show (e :: Ex.SomeException))
        Right _ -> return()
           
  threadDelay (15 * 1000 * 1000)
  routeLoop rc shapeEntities con
  
  
