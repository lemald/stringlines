{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPI where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.ContentTypes

-- Create a new type to accept the content-type returned by the MBTA
data TJSON = TJSON JSON

instance FromJSON a => MimeUnrender TJSON a where
  mimeUnrender _ = eitherDecodeLenient

instance Accept TJSON where
  contentType _ = "application" // "vnd.api+json"
                  /: ("charset", "utf-8")

type VehicleAPI = "vehicles"
                  :> QueryParam "api-key" Text
                  :> QueryParam "filter[route]" RouteID
                  :> Get '[TJSON] APIResponse

type RouteID = Text

data APIResponse = APIResponse {
  payload :: [Vehicle]
} deriving (Generic, Show)

data Vehicle = Vehicle {
  id :: Text,
  attributes :: VehicleAttributes
} deriving (Generic, Show)

data VehicleAttributes = VehicleAttributes {
  current_status :: Text
} deriving (Generic, Show)

-- This is necessary due to "data" being a keyword in Haskell
instance FromJSON APIResponse where
  parseJSON = withObject "apiresponse" $ \o -> do
    payload <- o .: "data"
    return APIResponse{..}

instance FromJSON Vehicle
instance FromJSON VehicleAttributes
