{-# LANGUAGE OverloadedStrings #-}

module DataStore where

import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import Client

instance FromRow TripInfo where
  fromRow = TripInfo <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TripInfo where
  toRow TripInfo{trip_id = trip_id
                ,route_id = route_id
                ,direction_id = direction_id
                ,latitude = latitude
                ,longitude = longitude
                ,timestamp = timestamp} =
    toRow (trip_id, route_id, direction_id, latitude, longitude, timestamp)

connectToDB :: IO(Connection)
connectToDB = open "locations.db"

closeDBCon :: Connection -> IO()
closeDBCon con = close con

createTables :: Connection -> IO()
createTables con =
  execute_ con "CREATE TABLE IF NOT EXISTS locations (trip_id STR, route_id STR, direction_id INTEGER, latitude REAL, longitude REAL, timestamp STR)"

insertTripInfo :: Connection -> [TripInfo] -> IO()
insertTripInfo con ts =
  executeMany
  con
  "INSERT INTO locations (trip_id, route_id, direction_id, latitude, longitude, timestamp) SELECT ?,?,?,?,?,? WHERE NOT EXISTS (SELECT 1 FROM locations WHERE trip_id = ? AND timestamp = ?)"
  (fmap (\t -> (trip_id t, route_id t, direction_id t, latitude t, longitude t, timestamp t, trip_id t, timestamp t)) ts)
