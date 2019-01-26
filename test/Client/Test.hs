{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Client.Test where

import Data.Time.Calendar
import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit

import Config
import Client
import TAPI

clientTests :: TestTree
clientTests = testGroup "Client"
  [testCase "tripInfoFromVehicle in good case" $
    tripInfoFromVehicle (NoShapeConf "77") goodVehicleEntity @?= Just goodVehicleTripInfo
  ,testCase "tripInfoFromVehicle in bad case" $
    tripInfoFromVehicle (NoShapeConf "77") badVehicleEntity @?= Nothing
  ,testCase "tripInfoFromResponse" $
    tripInfoFromResponse apiResponse (NoShapeConf "77") @?= [goodVehicleTripInfo]
  ]

time :: UTCTime
time = read "2018-12-01 20:30:00"

goodVehicle :: Vehicle
goodVehicle = Vehicle{
  current_status = "foo"
  ,current_stop_sequence = Just 7
  ,speed = 10.0
  ,bearing = Just 17
  ,label = "bar"
  ,direction_id = 1
  ,latitude = 2.0
  ,longitude = 3.0
  ,updated_at = time
  }

goodVehicleEntity :: Entity Vehicle
goodVehicleEntity = Entity{
  id = "1234"
  ,attributes = goodVehicle
  ,relationships = Relationships{
      route = Just Relationship{
          payload = RelationshipPayload{
              id = "77"
              }
          }
      ,trip = Just Relationship{
          payload = RelationshipPayload{
              id = "1234"
              }
          }
      ,vehicle = Nothing
      }
  }

badVehicle :: Vehicle
badVehicle = Vehicle{
  current_status = "foo"
  ,current_stop_sequence = Just 7
  ,speed = 10.0
  ,bearing = Just 17
  ,label = "bar"
  ,direction_id = 1
  ,latitude = 2.0
  ,longitude = 3.0
  ,updated_at = time
  }

badVehicleEntity :: Entity Vehicle
badVehicleEntity = Entity{
  id = "1234"
  ,attributes = badVehicle
  ,relationships = Relationships{
      route = Just Relationship{
          payload = RelationshipPayload{
              id = "77"
              }
          }
      ,trip = Nothing
      ,vehicle = Nothing
      }
  }

apiResponse :: APIResponse (Entity Vehicle)
apiResponse = APIResponse{
  api_response_data = [goodVehicleEntity, badVehicleEntity]
  }

goodVehicleTripInfo :: TripInfo
goodVehicleTripInfo = TripInfo{
  trip_id = "1234"
  ,route_id = "77"
  ,direction_id = 1
  ,latitude = 2.0
  ,longitude = 3.0
  ,progress = Nothing
  ,timestamp = time
  }

