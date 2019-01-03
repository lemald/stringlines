module Progress where

import TAPI
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
import Data.Geo.Jord.Length
import Data.Geo.Jord.Quantity
import Data.List
import Data.Text as T
import GPolyline

progressOnRoute :: Vehicle -> Shape -> Maybe Double
progressOnRoute vehicle shape = let
  vehicle_pos = (decimalLatLong
                 (TAPI.latitude vehicle)
                 (TAPI.longitude vehicle))
  points = shapeToPoints shape
  closestPoint = closestPointAlongRoute vehicle_pos points
  in case closestPoint of
       Nothing -> Nothing
       Just p -> let
         (covered, total, _, _) =
           Data.List.foldl'
           (progressOnRoute' p)
           (metres 0.0, metres 0.0, Nothing, False)
           points
         in Just (toMetres covered / toMetres total)

progressOnRoute' :: LatLong ->
                    (Length, Length, Maybe LatLong, Bool) ->
                    LatLong ->
                    (Length, Length, Maybe LatLong, Bool)
progressOnRoute' _ (_, _, Nothing, _) p =
  (metres 0.0, metres 0.0, Just p, False)
progressOnRoute' cp (vehicleDist, totalDist, Just p1, pointSeen) p2 =
  let dist = surfaceDistance84 p1 p2
      vehicleDist' = if pointSeen
                     then vehicleDist `add` dist
                     else vehicleDist
      totalDist' = totalDist `add` dist
      pointSeen' = pointSeen || (p2 == cp)
  in (vehicleDist', totalDist', Just p2, pointSeen')

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
