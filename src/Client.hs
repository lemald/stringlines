{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Client (
  getVehicles
  ,getShapes
  ,queryAPI
  ,tripInfoFromResponse
  ,tripInfoFromVehicle
  ,attributesByID
  ,TripInfo(..)
  ) where

import Data.Proxy
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.API

import Config
import TAPI
import Progress

api :: Proxy TAPI
api = Proxy

vehicles :: Maybe T.Text       -- API key
         -> Maybe TAPI.RouteID -- Route ID
         -> ClientM (APIResponse (Entity Vehicle))

shapes :: Maybe T.Text       -- API key
       -> Maybe TAPI.RouteID -- Route ID
       -> ClientM (APIResponse (Entity Shape))

vehicles :<|> shapes = client api

getVehicles :: TAPI.RouteID -> T.Text -> ClientM (APIResponse (Entity Vehicle))
getVehicles route k = vehicles (Just k) (Just route)

getShapes :: TAPI.RouteID -> T.Text -> ClientM (APIResponse (Entity Shape))
getShapes route k = shapes (Just k) (Just route)

queryAPI :: T.Text ->
            (T.Text -> ClientM (APIResponse a)) ->
            IO (Either ClientError (APIResponse a))
queryAPI apiKey queryFunc = do
  manager <- newManager tlsManagerSettings
  res <- runClientM
    (queryFunc apiKey)
    (mkClientEnv manager (BaseUrl Https "api-v3.mbta.com" 443 ""))
  return res

data TripInfo = TripInfo {
  trip_id :: TripID
  ,route_id :: RouteID
  ,direction_id :: DirectionID
  ,vehicle_label :: T.Text
  ,latitude :: Double
  ,longitude :: Double
  ,progress :: Maybe Double
  ,timestamp :: UTCTime
} deriving (Show, Eq)

tripInfoFromResponse :: APIResponse (Entity Vehicle) ->
                        RouteConf ->
                        [TripInfo]
tripInfoFromResponse res cfg = do
  ts <- (fmap (tripInfoFromVehicle cfg) $ api_response_data res)
  case ts of
    Nothing -> []
    Just a -> [a]

tripInfoFromVehicle :: RouteConf -> Entity Vehicle -> Maybe TripInfo
tripInfoFromVehicle cfg Entity{
  attributes = Vehicle{
      direction_id = direction_id
      ,latitude = lat
      ,longitude = lon
      ,label = label
      ,updated_at = ts
      }
  ,relationships = Relationships{
      route = Just Relationship{
          payload = RelationshipPayload{
              id = route_id
              }
          },
      trip = Just Relationship{
          payload = RelationshipPayload{
              id = trip_id
              }
          }
      }
  } = Just TripInfo{
  trip_id = trip_id
  ,route_id = route_id
  ,direction_id = direction_id
  ,vehicle_label = label
  ,latitude = lat
  ,longitude = lon
  ,progress = case routeConfShape cfg direction_id of
      Just shape -> progressOnRoute lat lon shape
      Nothing -> Nothing
  ,timestamp = ts
  }
tripInfoFromVehicle _ _ = Nothing
