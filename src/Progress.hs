module Progress where

import TAPI
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.List
import Data.Text as T
import GPolyline

progressOnRoute :: Vehicle -> Shape -> Double
progressOnRoute vehicle shape = let
  vehicle_pos = (decimalLatLong
                 (TAPI.latitude vehicle)
                 (TAPI.longitude vehicle))
  points = shapeToPoints shape
  in 1.0 -- dummy double to make it typecheck while WIP

shapeToPoints :: Shape -> [LatLong]
shapeToPoints s = let
  line = decodeline (T.unpack $ polyline s)
  in fmap (\(lat, lon) -> decimalLatLong lat lon) line

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

-- I'm not sure this will actually be necessary, but now that I've got
-- all of the logic right I don't want to throw it away without
-- committing it for future reference.
pointsToArcs :: [LatLong] -> [GreatArc]
pointsToArcs ps = let
  (as, _) = Data.List.foldl' pointsToArcs' ([], Nothing) ps
  in as

pointsToArcs' :: ([GreatArc], Maybe LatLong) ->
                 LatLong ->
                 ([GreatArc], Maybe LatLong)
pointsToArcs' (as, Nothing) point = (as, Just point)
pointsToArcs' (as, Just prevPoint) newPoint =
  case greatArcE (prevPoint, newPoint) of
    Right a -> (as ++ [a], Just newPoint)
    Left _ -> (as, Just newPoint)
