{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson.Types
import Data.Yaml
import qualified Data.Text as T
import GHC.Generics

import TAPI

dropFieldOptions :: Int -> Options
dropFieldOptions n = defaultOptions { fieldLabelModifier = drop n }

data RouteCfg = RouteCfg {
  route_cfg_id :: TAPI.RouteID
  ,route_cfg_dir0_shape_id :: Maybe TAPI.ShapeID
  ,route_cfg_dir1_shape_id :: Maybe TAPI.ShapeID
  } deriving (Generic, Show)

instance FromJSON RouteCfg where
  parseJSON = genericParseJSON $ dropFieldOptions 10

data Config = Config {
  cfg_api_key :: String
  ,cfg_routes :: [RouteCfg]
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ dropFieldOptions 4

readConfig :: FilePath -> IO(Either String Config)
readConfig fp = do
  decodeRes <- decodeFileEither fp
  return $ case decodeRes of
             Left pe -> Left $ prettyPrintParseException pe
             Right c -> Right c
