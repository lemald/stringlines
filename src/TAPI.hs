{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPI where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
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

type TAPI = "vehicles"
            :> QueryParam "api-key" Text
            :> QueryParam "filter[route]" RouteID
            :> Get '[TJSON] (APIResponse (Entity Vehicle))
            :<|> "shapes"
            :> QueryParam "api-key" Text
            :> QueryParam "filter[route]" RouteID
            :> Get '[TJSON] (APIResponse (Entity Shape))

type RouteID = Text
type VehicleID = Text
type TripID = Text

data APIResponse a = APIResponse {
  payload :: [a]
} deriving (Generic, Show)

data Entity a = Entity {
  id :: Text,
  attributes :: a,
  relationships :: Relationships
} deriving (Generic, Show)

data Relationships = Relationships {
  route :: Maybe (Relationship RouteID),
  vehicle :: Maybe (Relationship VehicleID),
  trip :: Maybe (Relationship TripID)
} deriving (Generic, Show)

data Relationship a = Relationship {
  payload :: RelationshipPayload a
} deriving (Generic, Show)

data RelationshipPayload a = RelationshipPayload {
  id :: a
} deriving (Generic, Show)

data Vehicle = Vehicle {
  current_status :: Text,
  current_stop_sequence :: Int,
  speed :: Double,
  direction_id :: Int,
  bearing :: Maybe Int,
  label :: Text,
  longitude :: Double,
  latitude :: Double,
  updated_at :: UTCTime
} deriving (Generic, Show)

data Shape = Shape {
  polyline :: Text,
  direction_id :: Int
} deriving (Generic, Show)

-- This is necessary due to "data" being a keyword in Haskell
instance FromJSON a => FromJSON (APIResponse a) where
  parseJSON = withObject "apiresponse" $ \o -> do
    payload <- o .: "data"
    return APIResponse{..}
instance FromJSON a => FromJSON (Entity a)
instance FromJSON Relationships
instance FromJSON a => FromJSON (RelationshipPayload a)
instance FromJSON a => FromJSON (Relationship a) where
  parseJSON = withObject "relationship" $ \o -> do
    payload <- o .: "data"
    return Relationship{..}
instance FromJSON Vehicle
instance FromJSON Shape
