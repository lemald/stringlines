{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DataStore (
  connectToDB
  ,closeDBCon
  ,createTables
  ,insertTripInfo
  ,tripInfoByRouteForDay
  ) where

import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Client
import TAPI

instance FromRow TripInfo where
  fromRow = TripInfo <$> field <*> field <*> field
            <*> field <*> field <*> field <*> field

instance ToRow TripInfo where
  toRow TripInfo{trip_id = trip_id
                ,route_id = route_id
                ,direction_id = direction_id
                ,latitude = latitude
                ,longitude = longitude
                ,progress = progress
                ,timestamp = timestamp} =
    toRow (trip_id, route_id, direction_id,
           latitude, longitude, progress, timestamp)

connectToDB :: IO(Connection)
connectToDB = open "locations.db"

closeDBCon :: Connection -> IO()
closeDBCon con = close con

createTables :: Connection -> IO()
createTables con = do
  execute_ con "CREATE TABLE IF NOT EXISTS location (trip_id STR, route_id STR, direction_id INTEGER, latitude REAL, longitude REAL, progress REAL, timestamp STR)"
  execute_ con "CREATE INDEX IF NOT EXISTS location_trip_id ON location (trip_id)"
  execute_ con "CREATE INDEX IF NOT EXISTS location_route_id ON location (route_id)"
  execute_ con "CREATE INDEX IF NOT EXISTS location_timestamp ON location (DATETIME(timestamp))"

insertTripInfo :: Connection -> [TripInfo] -> IO()
insertTripInfo con ts =
  executeMany
  con
  "INSERT INTO location (trip_id, route_id, direction_id, latitude, longitude, progress, timestamp) SELECT ?,?,?,?,?,?,? WHERE NOT EXISTS (SELECT 1 FROM location WHERE trip_id = ? AND timestamp = ?)"
  (fmap insertTripInfoQueryArgs ts)

insertTripInfoQueryArgs :: TripInfo ->
                           (TAPI.TripID, TAPI.RouteID, TAPI.DirectionID,
                            Double, Double, Maybe Double, UTCTime,
                            TAPI.TripID, UTCTime)
insertTripInfoQueryArgs TripInfo{
  trip_id = trip_id
  ,route_id = route_id
  ,direction_id = direction_id
  ,latitude = latitude
  ,longitude = longitude
  ,progress = progress
  ,timestamp = timestamp
  } = (trip_id , route_id, direction_id,
       latitude, longitude, progress,
       timestamp, trip_id, timestamp)

tripInfoByRouteForDay :: Connection ->
                         TAPI.RouteID ->
                         Day ->
                         TimeZone ->
                         IO([TripInfo])
tripInfoByRouteForDay con routeID day tz =
  let
    startTime = LocalTime{
      localDay = day
      ,localTimeOfDay = TimeOfDay{
          todHour = 3
          ,todMin = 30
          ,todSec = 0
          }
      }
    endTime = LocalTime{
      localDay = addDays 1 day
      ,localTimeOfDay = TimeOfDay{
          todHour = 2
          ,todMin = 30
          ,todSec = 0
          }
      }
    in query
       con
       "SELECT CAST(trip_id AS TEXT), CAST(route_id AS TEXT), direction_id, latitude, longitude, progress, timestamp FROM location WHERE route_id = ? AND DATETIME(timestamp) > DATETIME(?) AND DATETIME(timestamp) < DATETIME(?) ORDER BY DATETIME(timestamp)"
       (routeID, localTimeToUTC tz startTime, localTimeToUTC tz endTime)
  
