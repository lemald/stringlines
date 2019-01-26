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
  cfg <- liftEither confRes

  routes <- mapM (\rc ->
                    if maybe False (\x -> length x > 2) (route_cfg_shape_ids rc)
                    then throwError ("More than 2 shape IDs specified for route " ++
                                     (T.unpack $ route_cfg_id rc))
                    else return (route_cfg_id rc)
                 )
            (cfg_routes cfg)

  routeShapes <- mapM (\r -> do
                          shapeResponse <- liftIO $
                                           queryAPI (cfg_api_key cfg) $
                                           getShapes r
                          apiResponse <- case shapeResponse of
                            Left e -> throwError
                              ("Error fetching shape data from API for route " ++
                               T.unpack r ++ ": " ++ show e)
                            Right res -> return res
                          return (r, api_response_data apiResponse))
                 routes

  routeConfs <- mapM (createRouteConf cfg) routeShapes

  liftIO $ do
    runThreads (cfg_api_key cfg) routeConfs
    infoM "stringlines.poll" "All child threads exited, shutting down."    

runThreads :: T.Text -> [RouteConf] -> IO()
runThreads apiKey routeConfs = do
  con <- connectToDB
  createTables con

  mvars <- mapM (\r -> do
                    mvar <- newEmptyMVar
                    thread <- forkFinally
                      (initiateRouteLoop r con apiKey)
                      (putMVar mvar)
                    return mvar)
           routeConfs
  mapM_ (\m -> takeMVar m) mvars
  closeDBCon con

initiateRouteLoop :: RouteConf -> Connection -> T.Text -> IO()
initiateRouteLoop rc con apiKey = do
  shapeResponse <- queryAPI apiKey $ getShapes (routeConfRoute rc)
  case shapeResponse of
    (Left err) -> errorM "stringlines.poll" ("Error fetching shape data from API: "
                                              ++ show err)
    (Right apires) -> let shapeEntities = api_response_data apires
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
      sql_res <- Ex.try (let tripInfo = tripInfoFromResponse apires rc
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
        Right _ -> return ()
           
  threadDelay (15 * 1000 * 1000)
  routeLoop apiKey rc shapeEntities con
  
  
