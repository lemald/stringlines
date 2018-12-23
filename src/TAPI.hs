{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TAPI where

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Time (UTCTime)
import Servant.API

type VehicleAPI = "vehicles" :> QueryParam "api-key" Text :> QueryParam "filter[route]" RouteID :> Get '[JSON] APIResponse

type RouteID = Text

data APIResponse = APIResponse {
  payload :: [Vehicle]
} deriving (Generic, Show)

data Vehicle = Vehicle {
  id :: Text
} deriving (Generic, Show)

instance FromJSON APIResponse where
  parseJSON = withObject "apiresponse" $ \o -> do
    payload <- o .: "data"
    return APIResponse{..}

instance FromJSON Vehicle
