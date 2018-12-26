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

createTables :: Connection -> IO()
createTables con =
  execute_ con "CREATE TABLE IF NOT EXISTS locations (trip_id STR, route_id STR, direction_id INTEGER, latitude REAL, longitude REAL, timestamp STR)"

insertTripInfo :: Connection -> TripInfo -> IO()
insertTripInfo con t =
  execute con "INSERT INTO locations (trip_id, route_id, direction_id, latitude, longitude, timestamp) values (?,?,?,?,?,?)" t
