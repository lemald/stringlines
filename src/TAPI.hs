{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPI where

import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.ContentTypes

data TJSON = TJSON JSON

instance FromJSON a => MimeUnrender TJSON a where
  mimeUnrender _ = eitherDecodeLenient

instance Accept TJSON where
  contentType _ = "application" // "vnd.api+json" /: ("charset", "utf-8")

type VehicleAPI = "vehicles" :> QueryParam "api-key" Text :> QueryParam "filter[route]" RouteID :> Get '[TJSON] APIResponse

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
