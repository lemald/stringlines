{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import TAPI
import Data.Proxy
import Data.Text
import Servant.Client
import Servant.API

api :: Proxy TAPI.VehicleAPI
api = Proxy

vehicles :: Text -- API key
         -> Maybe TAPI.RouteID -- Route ID
         -> ClientM APIResponse

vehicles = client api
