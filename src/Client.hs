{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Client where

import TAPI
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.API

api :: Proxy TAPI
api = Proxy

vehicles :: Maybe Text         -- API key
         -> Maybe TAPI.RouteID -- Route ID
         -> ClientM (APIResponse (Entity Vehicle))

shapes :: Maybe Text         -- API key
       -> Maybe TAPI.RouteID -- Route ID
       -> ClientM (APIResponse (Entity Shape))

vehicles :<|> shapes = client api

apiKey :: Text
apiKey = "***REMOVED***"

getVehicles :: TAPI.RouteID -> ClientM (APIResponse (Entity Vehicle))
getVehicles route = vehicles (Just apiKey) (Just route)

getShapes :: TAPI.RouteID -> ClientM (APIResponse (Entity Shape))
getShapes route = shapes (Just apiKey) (Just route)

queryAPI :: ClientM (APIResponse a) -> IO (Either ServantError (APIResponse a))
queryAPI queryFunc = do
  manager <- newManager tlsManagerSettings
  res <- runClientM
    queryFunc
    (mkClientEnv manager (BaseUrl Https "api-v3.mbta.com" 443 ""))
  return res

data TripInfo = TripInfo {
  trip_id :: TripID,
  route_id :: RouteID,
  direction_id :: DirectionID,
  latitude :: Double,
  longitude :: Double,
  timestamp :: UTCTime
} deriving (Show, Eq)

tripInfoFromResponse :: APIResponse (Entity Vehicle) -> [TripInfo]
tripInfoFromResponse APIResponse{ payload = vs } =
  (fmap tripInfoFromVehicle vs) >>= \ts -> case ts of
                                             Nothing -> []
                                             Just a -> [a]

tripInfoFromVehicle :: Entity Vehicle -> Maybe TripInfo
tripInfoFromVehicle Entity{
  attributes = Vehicle{
      direction_id = direction_id
      ,latitude = lat
      ,longitude = lon
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
  ,latitude = lat
  ,longitude = lon
  ,timestamp = ts
  }
tripInfoFromVehicle _ = Nothing
