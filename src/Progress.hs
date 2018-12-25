module Progress where

import TAPI
import Data.Geo.Jord.Geodetics
import Data.Geo.Jord.LatLong
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
