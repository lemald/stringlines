{-# LANGUAGE OverloadedStrings #-}

module DataStore.Test where

import Database.SQLite.Simple
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Tasty
import Test.Tasty.HUnit

import Client
import DataStore

testDBConnection :: IO(Connection)
testDBConnection = open ":memory:"

dataStoreTests :: TestTree
dataStoreTests = testGroup "DataStore"
  [testCase "Creates table" $ testDBConnection >>= testTableCreation >>= close
  ,testCase "Inserts row" $ testDBConnection >>= testRowInsert >>= close
  ,testCase "Don't insert duplicate data points" $
    testDBConnection >>= testNoDups >>= close
  ,testCase "tripInfoByRouteforday" $
    testDBConnection >>= testTripInfoByRouteForDay >>= close]

testTableCreation :: Connection -> IO(Connection)
testTableCreation con = do
  createTables con
  tables <- query_
    con
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'location'"
    :: IO([Only String])
  length tables @?= 1
  return con

testRowInsert :: Connection -> IO(Connection)
testRowInsert con = do
  createTables con
  insertTripInfo con [tripInfo1]
  tripInfoEntries <- query_
    con
    "SELECT CAST(trip_id AS TEXT), CAST(route_id AS TEXT), direction_id, latitude, longitude, progress, timestamp FROM location"
    :: IO([TripInfo])
  length tripInfoEntries @?= 1
  head tripInfoEntries @?= tripInfo1
  return con

testNoDups :: Connection -> IO(Connection)
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
  return con

testTripInfoByRouteForDay :: Connection -> IO(Connection)
testTripInfoByRouteForDay con = do
  createTables con
  insertTripInfo con [tripInfo1, tripInfo3, tripInfo4, tripInfo5]
  tripInfoEntries <- tripInfoByRouteForDay
                     con
                     "39"
                     [0]
                     (read "2018-12-01" :: Day)
                     (hoursToTimeZone (-5))
  length tripInfoEntries @?= 1
  head tripInfoEntries @?= tripInfo1
  return con

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

tripInfo3 :: TripInfo
tripInfo3 = TripInfo{
  trip_id = "123",
  route_id = "39",
  direction_id = 0,
  latitude = 3.0,
  longitude = 4.0,
  progress = Just 0.5,
  timestamp = read "2018-12-01 02:30:00" :: UTCTime
  }

tripInfo4 :: TripInfo
tripInfo4 = TripInfo{
  trip_id = "123",
  route_id = "77",
  direction_id = 0,
  latitude = 3.0,
  longitude = 4.0,
  progress = Just 0.5,
  timestamp = read "2018-12-01 16:30:00" :: UTCTime
  }

tripInfo5 :: TripInfo
tripInfo5 = TripInfo{
  trip_id = "123",
  route_id = "39",
  direction_id = 1,
  latitude = 1.0,
  longitude = 2.0,
  progress = Just 0.5,
  timestamp = read "2018-12-01 18:30:00" :: UTCTime
  }
