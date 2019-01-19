{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.Except
import Data.Either
import qualified Data.Text as T
import Database.SQLite.Simple
import GHC.IO.Handle.FD
import System.Exit
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler
import System.Log.Handler.Simple

import Client
import Config
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
    return $ setFormatter nh (simpleLogFormatter "[$time $prio] $msg")
  updateGlobalLogger rootLoggerName ((setHandlers [h]) .
                                     (System.Log.Logger.setLevel INFO))
  r <- runExceptT runner
  case r of
    Left e -> do
      errorM "stringlines.poll" e
      exitWith $ ExitFailure 1
    Right () -> return ()

runner :: ExceptT String IO ()
runner = do
  confRes <- liftIO $ readConfig "config.yaml"
  cfg <- case confRes of
           Left e -> throwError
                     ("Couldn't parse configuration file: " ++ e)
           Right c -> return c
  -- Further validation of config (mainly around shape IDs) and
  -- translation to some better inernal structure goes here

  liftIO $ runThreads cfg

  liftIO $ infoM "stringlines.poll" "All child threads exited, shutting down."

runThreads :: Config -> IO()
runThreads cfg = do
  con <- liftIO $ connectToDB
  liftIO $ createTables con

  mvars <- mapM (\r -> do
                    mvar <- newEmptyMVar
                    thread <- forkFinally
                      (initiateRouteLoop r con (cfg_api_key cfg))
                      (putMVar mvar)
                    return mvar)
           routes
  mapM_ (\m -> takeMVar m) mvars
  closeDBCon con

initiateRouteLoop :: RouteConf -> Connection -> T.Text -> IO()
initiateRouteLoop rc con apiKey = do
  shapeResponse <- queryAPI apiKey $ getShapes (routeConfRoute rc)
  case shapeResponse of
    (Left err) -> errorM "stringlines.poll" ("Error fetching shape data from API: "
                                              ++ show err)
    (Right apires) -> let shapeEntities = entitiesFromResponse apires
                      in routeLoop apiKey rc shapeEntities con

routeLoop :: T.Text -> RouteConf -> [Entity Shape] -> Connection -> IO()
routeLoop apiKey rc shapeEntities con = do
  res <- queryAPI apiKey $ getVehicles (routeConfRoute rc)
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
  routeLoop apiKey rc shapeEntities con
  
  
