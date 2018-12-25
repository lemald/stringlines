{-# LANGUAGE OverloadedStrings #-}

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
  tripid :: TripID,
  routeid :: RouteID,
  latitude :: Double,
  longitude :: Double,
  timestamp :: UTCTime
} deriving Show
