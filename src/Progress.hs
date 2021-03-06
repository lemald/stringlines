{-# LANGUAGE DuplicateRecordFields #-}

module Progress (
  progressOnRoute
  ,shapeToPoints
  ,closestPointAlongRoute
  ) where

import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.Quantity
import Data.List
import Data.Text as T
import GPolyline

import TAPI

progressOnRoute :: Double -> Double -> Shape -> Maybe Double
progressOnRoute vehicleLat vehicleLong shape = let
  vehiclePos = decimalLatLong vehicleLat vehicleLong
  points = shapeToPoints shape
  closestPoint = closestPointAlongRoute vehiclePos points
  in case closestPoint of
       Nothing -> Nothing
       Just p -> let
         (covered, total, _, _) =
           Data.List.foldl'
           (progressOnRoute' p)
           (metres 0.0, metres 0.0, Nothing, False)
           points
         in Just $ invertProgress (toMetres covered / toMetres total) shape

progressOnRoute' :: LatLong ->
                    (Length, Length, Maybe LatLong, Bool) ->
                    LatLong ->
                    (Length, Length, Maybe LatLong, Bool)
progressOnRoute' cp (_, _, Nothing, _) p =
  (metres 0.0, metres 0.0, Just p, cp == p)
progressOnRoute' cp (vehicleDist, totalDist, Just p1, pointSeen) p2 =
  let dist = surfaceDistance84 p1 p2
      vehicleDist' = if pointSeen
                     then vehicleDist
                     else vehicleDist `add` dist
      totalDist' = totalDist `add` dist
      pointSeen' = pointSeen || (p2 == cp)
  in (vehicleDist', totalDist', Just p2, pointSeen')

invertProgress :: Double -> Shape -> Double
invertProgress p s = case shapeDirectionID s of
  0 -> p
  1 -> 1 - p

shapeToPoints :: TAPI.Shape -> [LatLong]
shapeToPoints s = let
  line = decodeline (T.unpack $ polyline s)
  in fmap (uncurry decimalLatLong) line

shapeDirectionID :: TAPI.Shape -> TAPI.DirectionID
shapeDirectionID TAPI.Shape{ direction_id = d } = d

closestPointAlongRoute :: LatLong -> [LatLong] -> Maybe LatLong
closestPointAlongRoute pos ps = let
  (p, _) = Data.List.foldl' (closestPointAlongRoute' pos) (Nothing, Nothing) ps
  in p

closestPointAlongRoute' :: LatLong ->
                           (Maybe LatLong, Maybe Length) ->
                           LatLong ->
                           (Maybe LatLong, Maybe Length)
closestPointAlongRoute' pos (closest, closestDist) nextPoint =
  let dist = surfaceDistance84 pos nextPoint
  in if toMetres dist > 250
     then (closest, closestDist)
     else case closestDist of
            Nothing -> (Just nextPoint, Just dist)
            Just a -> if toMetres dist < toMetres a
                      then (Just nextPoint, Just dist)
                      else (closest, closestDist)
