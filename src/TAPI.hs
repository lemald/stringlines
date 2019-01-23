{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPI (
  TAPI
  ,RouteID
  ,VehicleID
  ,TripID
  ,ShapeID
  ,DirectionID
  ,APIResponse(
      api_response_data
      ,APIResponse
      )
  ,Entity(
      id
      ,attributes
      ,relationships
      ,Entity
      )
  ,Relationships(
      route
      ,vehicle
      ,trip
      ,Relationships
      )
  ,Relationship(
      payload
      ,Relationship
      )
  ,RelationshipPayload(
      id
      ,RelationshipPayload
      )
  ,Vehicle(
      current_status
      ,current_stop_sequence
      ,speed
      ,direction_id
      ,bearing
      ,label
      ,longitude
      ,latitude
      ,updated_at
      ,Vehicle
      )
  ,Shape(
      polyline
      ,name
      ,direction_id
      ,Shape
      )
  ,attributesByID
  ) where

import Data.Aeson
import qualified Data.Text as T
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
            :> QueryParam "api_key" T.Text
            :> QueryParam "filter[route]" RouteID
            :> Get '[TJSON] (APIResponse (Entity Vehicle))
            :<|> "shapes"
            :> QueryParam "api_key" T.Text
            :> QueryParam "filter[route]" RouteID
            :> Get '[TJSON] (APIResponse (Entity Shape))

type RouteID = T.Text
type VehicleID = T.Text
type TripID = T.Text
type ShapeID = T.Text
type DirectionID = Int

dropFieldOptions :: Int -> Options
dropFieldOptions n = defaultOptions { fieldLabelModifier = drop n }

data APIResponse a = APIResponse {
  api_response_data :: [a]
} deriving (Generic, Show)

data Entity a = Entity {
  id :: T.Text,
  attributes :: a,
  relationships :: Relationships
} deriving (Generic, Eq, Show)

data Relationships = Relationships {
  route :: Maybe (Relationship RouteID),
  vehicle :: Maybe (Relationship VehicleID),
  trip :: Maybe (Relationship TripID)
} deriving (Generic, Eq, Show)

data Relationship a = Relationship {
  payload :: RelationshipPayload a
} deriving (Generic, Eq, Show)

data RelationshipPayload a = RelationshipPayload {
  id :: a
} deriving (Generic, Eq, Show)

data Vehicle = Vehicle {
  current_status :: T.Text,
  current_stop_sequence :: Int,
  speed :: Double,
  direction_id :: DirectionID,
  bearing :: Maybe Int,
  label :: T.Text,
  longitude :: Double,
  latitude :: Double,
  updated_at :: UTCTime
} deriving (Generic, Eq, Show)

data Shape = Shape {
  polyline :: T.Text,
  name :: T.Text,
  direction_id :: Int
} deriving (Generic, Show, Eq)

instance FromJSON a => FromJSON (APIResponse a) where
  parseJSON = genericParseJSON $ dropFieldOptions 13
instance FromJSON a => FromJSON (Entity a)
instance FromJSON Relationships
instance FromJSON a => FromJSON (RelationshipPayload a)
instance FromJSON a => FromJSON (Relationship a) where
  parseJSON = withObject "relationship" $ \o -> do
    payload <- o .: "data"
    return Relationship{..}
instance FromJSON Vehicle
instance FromJSON Shape

attributesByID :: [Entity a] -> T.Text -> Maybe a
attributesByID [] _ = Nothing
attributesByID (Entity{ id = entityID, attributes = attr }:es) id =
  if entityID == id then Just attr
  else attributesByID es id
