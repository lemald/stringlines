{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module TAPI where

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Time (UTCTime)
import Servant.API

type VehicleAPI = Capture "APIKey" Text :> "vehicles" :> QueryParam "filter[route]" RouteID :> Get '[JSON] [Vehicles]

type RouteID = Text

data Vehicles = Vehicles {
  foo :: String
} deriving Generic

instance FromJSON Vehicles
