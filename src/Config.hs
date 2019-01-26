{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (
  routeConfRoute
  ,routeConfShape
  ,RouteConf(..)
  ,Config(..)
  ,RouteCfg(..)
  ,createRouteConf
  ,readConfig
  ) where

import Control.Monad.Except
import Data.Aeson.Types
import Data.List
import Data.Yaml
import qualified Data.Text as T
import GHC.Generics

import TAPI

data RouteConf = NoShapeConf RouteID |
                 SingleShapeConf RouteID TAPI.Shape |
                 TwoShapeConf RouteID TAPI.Shape TAPI.Shape
               deriving (Show, Eq)

routeConfRoute :: RouteConf -> RouteID
routeConfRoute (SingleShapeConf r _) = r
routeConfRoute (TwoShapeConf r _ _) = r

-- DirectionID should perhaps be rewritten to have just two
-- constructors so that this is a complete enumeration of the
-- possibilities
routeConfShape :: RouteConf -> DirectionID -> Maybe Shape
routeConfShape (NoShapeConf _) _ = Nothing
routeConfShape (SingleShapeConf _ s) _ = Just s
routeConfShape (TwoShapeConf _ s _) 0 = Just s
routeConfShape (TwoShapeConf _ _ s) 1 = Just s

dropFieldOptions :: Int -> Options
dropFieldOptions n = defaultOptions { fieldLabelModifier = drop n }

data RouteCfg = RouteCfg {
  route_cfg_id :: TAPI.RouteID
  ,route_cfg_shape_ids :: Maybe [TAPI.ShapeID]
  } deriving (Generic, Show)

instance FromJSON RouteCfg where
  parseJSON = genericParseJSON $ dropFieldOptions 10

data Config = Config {
  cfg_api_key :: T.Text
  ,cfg_routes :: [RouteCfg]
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ dropFieldOptions 4

readConfig :: FilePath -> IO(Either String Config)
readConfig fp = do
  decodeRes <- decodeFileEither fp
  return $ case decodeRes of
             Left pe -> Left $
               "Couldn't parse configuration file: " ++
               prettyPrintParseException pe
             Right c -> Right c

createRouteConf :: (Monad m) =>
                   Config ->
                   (RouteID, [TAPI.Entity TAPI.Shape]) ->
                   ExceptT String m RouteConf
createRouteConf cfg (routeID, shapeEntities) = do
  let maybeRouteCfg = find (\c -> route_cfg_id c == routeID) (cfg_routes cfg)
  routeCfg <- case maybeRouteCfg of
                Nothing -> throwError $ "No route configuration for " ++
                           T.unpack routeID
                Just c -> return c
  case (route_cfg_shape_ids routeCfg) of
    Nothing -> return $ NoShapeConf routeID
    Just shape_ids -> do
      let maybeShapes = fmap (\i -> (i, attributesByID shapeEntities i)) shape_ids
      shapes <- mapM (\s -> case s of
                              (i, Nothing) -> throwError $
                                "No shape returned by API for ID " ++ T.unpack i
                              (_, Just s) -> return s
                         ) maybeShapes
      return $
        case take 2 shapes of
          [a, b] -> TwoShapeConf routeID a b
          [a] -> SingleShapeConf routeID a
          _ -> NoShapeConf routeID
