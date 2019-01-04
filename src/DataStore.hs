{-# LANGUAGE OverloadedStrings #-}

module DataStore where

import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Client

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
  (fmap (\t -> (trip_id t, route_id t, direction_id t,
                latitude t, longitude t, progress t,
                timestamp t, trip_id t, timestamp t)) ts)
