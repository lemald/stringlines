{-# LANGUAGE OverloadedStrings #-}

module DataStore.Test where

import Database.SQLite.Simple
import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit

import Client
import DataStore

testDBConnection :: IO(Connection)
testDBConnection = open ":memory:"

dataStoreTests :: TestTree
dataStoreTests = withResource
                 testDBConnection
                 (\c -> close c)
                 individualTests

individualTests :: IO(Connection) -> TestTree
individualTests con = testGroup "DataStore"
  [testCase "Creates table" $ con >>= testTableCreation
  ,testCase "Inserts row" $ con >>= testRowInsert
  ,testCase "Don't insert duplicate data points" $ con >>= testNoDups]

testTableCreation :: Connection -> IO()
testTableCreation con = do
  createTables con
  tables <- query_
    con
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'location'"
    :: IO([Only String])
  length tables @?= 1

testRowInsert :: Connection -> IO()
testRowInsert con = do
  createTables con
  insertTripInfo con [tripInfo1]
  tripInfoEntries <- query_
    con
    "SELECT CAST(trip_id AS TEXT), CAST(route_id AS TEXT), direction_id, latitude, longitude, progress, timestamp FROM location"
    :: IO([TripInfo])
  length tripInfoEntries @?= 1
  head tripInfoEntries @?= tripInfo1

testNoDups :: Connection -> IO()
testNoDups con = do
  createTables con
  insertTripInfo con [tripInfo1]
  insertTripInfo con [tripInfo2]
  tripInfoEntries <- query_
    con
    "SELECT CAST(trip_id AS TEXT), CAST(route_id AS TEXT), direction_id, latitude, longitude, progress, timestamp FROM location"
    :: IO([TripInfo])
  length tripInfoEntries @?= 1
  head tripInfoEntries @?= tripInfo1

tripInfo1 :: TripInfo
tripInfo1 = TripInfo{
  trip_id = "123",
  route_id = "39",
  direction_id = 0,
  latitude = 1.0,
  longitude = 2.0,
  progress = Just 0.5,
  timestamp = read "2018-12-01 20:30:00" :: UTCTime
  }

tripInfo2 :: TripInfo
tripInfo2 = TripInfo{
  trip_id = "123",
  route_id = "39",
  direction_id = 0,
  latitude = 3.0,
  longitude = 4.0,
  progress = Just 0.5,
  timestamp = read "2018-12-01 20:30:00" :: UTCTime
  }
