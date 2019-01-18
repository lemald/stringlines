{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as Ex
import Control.Monad
import qualified Data.Text as T
import Database.SQLite.Simple
import GHC.IO.Handle.FD
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler
import System.Log.Handler.Simple

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
  h <- do
    nh <- streamHandler stderr DEBUG
    return $ setFormatter nh (simpleLogFormatter "[$time $prio] $msg\n")
  updateGlobalLogger rootLoggerName ((setHandlers [h]) .
                                     (System.Log.Logger.setLevel INFO))

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

  infoM "stringlines.poll" "All child threads exited, shutting down."
  closeDBCon con

initiateRouteLoop :: RouteConf -> Connection -> IO()
initiateRouteLoop rc con = do
  shapeResponse <- queryAPI $ getShapes (routeConfRoute rc)
  case shapeResponse of
    (Left err) -> errorM "stringlines.poll" ("Error fetching shape data from API: "
                                              ++ show err)
    (Right apires) -> let shapeEntities = entitiesFromResponse apires
                      in routeLoop rc shapeEntities con

routeLoop :: RouteConf -> [Entity Shape] -> Connection -> IO()
routeLoop rc shapeEntities con = do
  res <- queryAPI $ getVehicles (routeConfRoute rc)
  case res of
    (Left err) -> errorM
                  "stringlines.poll"
                  ("Error fetching vehicle data from API: "
                   ++ show err)
    (Right apires) -> do
      sql_res <- Ex.try (let s = attributesByID shapeEntities
                                 $ routeConfShapeID rc
                             tripInfo = tripInfoFromResponse apires s
                         in do infoM
                                 "stringlines.poll"
                                 ("Fetched "
                                  ++ show (length tripInfo)
                                  ++ " locations for route "
                                  ++ T.unpack (routeConfRoute rc)
                                  ++ ".")
                               insertTripInfo con tripInfo)
      case sql_res of
        Left e -> errorM
                  "stringlines.poll"
                  ("Exception raised: "
                   ++ show (e :: Ex.SomeException))
        Right _ -> return()
           
  threadDelay (15 * 1000 * 1000)
  routeLoop rc shapeEntities con
  
  
