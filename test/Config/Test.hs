{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Config.Test where

import Control.Monad.Except
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Config
import TAPI

configTests :: TestTree
configTests = testGroup "Config" $
  [testCase "createRouteConf for a no-shape route" testNoShape
  ,testCase "createRouteConf for a single shape route" testSingleShape
  ,testCase "createRouteConf for a two shape route" testTwoShape
  ,testCase "createRouteConf for a missing shape" testMissingShape]

testNoShape :: IO()
testNoShape = do
  cfg <- runExceptT (createRouteConf testConfig ("1", testShapeEntities))
  cfg @?= (Right $ NoShapeConf "1")

testSingleShape :: IO()
testSingleShape = do
  cfg <- runExceptT (createRouteConf testConfig ("2", testShapeEntities))
  cfg @?= (Right $ SingleShapeConf "2" (makeTestShape "a"))

testTwoShape :: IO()
testTwoShape = do
  cfg <- runExceptT (createRouteConf testConfig ("3", testShapeEntities))
  cfg @?= (Right $ TwoShapeConf "3" (makeTestShape "b") (makeTestShape "c"))

testMissingShape :: IO()
testMissingShape = do
  cfg <- runExceptT (createRouteConf testConfig ("4", testShapeEntities))
  cfg @?= Left "No shape returned by API for ID e"

testConfig :: Config
testConfig = Config{
  cfg_api_key = "foo"
  ,cfg_routes = [
      RouteCfg{
          route_cfg_id = "1"
          ,route_cfg_shape_ids = Nothing
          }
      ,RouteCfg{
          route_cfg_id = "2"
          ,route_cfg_shape_ids = Just ["a"]
          }
      ,RouteCfg{
          route_cfg_id = "3"
          ,route_cfg_shape_ids = Just ["b", "c"]
          }
      ,RouteCfg{
          route_cfg_id = "4"
          ,route_cfg_shape_ids = Just ["d", "e"]
          }
      ]
  }

testRelationships :: TAPI.Relationships
testRelationships = TAPI.Relationships{
  route = Nothing
  ,vehicle = Nothing
  ,trip = Nothing
  }

makeTestShape :: T.Text -> TAPI.Shape
makeTestShape t = TAPI.Shape{
  polyline = ""
  ,name = ("Shape " <> t)
  ,direction_id = 0
  }

makeTestShapeEntity :: TAPI.ShapeID -> TAPI.Entity TAPI.Shape
makeTestShapeEntity id = TAPI.Entity{
  id = id
  ,attributes = makeTestShape id
  ,relationships = testRelationships
  }

testShapeEntities :: [TAPI.Entity TAPI.Shape]
testShapeEntities = fmap makeTestShapeEntity ["a", "b", "c", "d"]
