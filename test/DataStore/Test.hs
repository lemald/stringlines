{-# LANGUAGE OverloadedStrings #-}

module DataStore.Test where

import Database.SQLite.Simple
import Test.Tasty
import Test.Tasty.HUnit

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
  [testCase "Creates table" $ con >>= testTableCreation]

testTableCreation :: Connection -> IO()
testTableCreation con = do
  createTables con
  tables <- query_
    con
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name = 'location'"
    :: IO([Only String])
  length tables @?= 1
