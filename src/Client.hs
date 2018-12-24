{-# LANGUAGE OverloadedStrings #-}

module Client where

import TAPI
import Data.Proxy
import Data.Text
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS
import Servant.Client
import Servant.API

api :: Proxy TAPI.VehicleAPI
api = Proxy

vehicles :: Maybe Text         -- API key
         -> Maybe TAPI.RouteID -- Route ID
         -> ClientM VehicleResponse

vehicles = client api

apiKey :: Text
apiKey = "***REMOVED***"

getVehicles :: TAPI.RouteID -> ClientM (VehicleResponse)
getVehicles route = vehicles (Just apiKey) (Just route)

queryAPI :: ClientM (VehicleResponse) -> IO (Either ServantError VehicleResponse)
queryAPI queryFunc = do
  manager <- newManager tlsManagerSettings
  res <- runClientM queryFunc (mkClientEnv manager (BaseUrl Https "api-v3.mbta.com" 443 ""))
  return res
