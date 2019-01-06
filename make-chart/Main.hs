{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.Gnuplot.LineSpecification
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.PNG
import Graphics.Gnuplot.Time

import Client
import DataStore
import TAPI

main :: IO()
main = do
  con <- connectToDB
  -- TODO: Take this stuff in as arguments
  results <- tripInfoByRouteForDay con "77" (read "2019-01-04" :: Day) (hoursToTimeZone (-5))
  let paths = (resultsToPaths results)
  plotPathsStyle
    [Key Nothing
    ,XLabel "Time (UTC)"
    ,XTime
    ,XTicks $ Just ["600", "offset 0,graph 0.015"]
    ,YLabel "Progress along route"
    ,YRange (0, 1)
    ,Grid $ Just ["xtics", "ytics"]
    -- TODO: Output file should also come from args
    ,terminal $ Graphics.Gnuplot.Terminal.PNG.cons "output.png"
    ,Custom "terminal png size 15360,640" []
    ]
    paths

-- TODO: These next two functions should live in their own files and
-- have tests
resultsToPaths :: [TripInfo] -> [(PlotStyle, [(Double, Double)])]
resultsToPaths ts =
  let paths = Map.foldl' (flip (:)) [] $ accumTripInfoMap ts
  in fmap (\d ->
             (PlotStyle{plotType = Lines
                       ,lineSpec = CustomStyle []},
              prepXTime d)
          ) paths

accumTripInfoMap :: [TripInfo] -> Map.Map TAPI.TripID [(UTCTime, Double)]
accumTripInfoMap ts =
  Data.List.foldl'
  (\m t ->
     case progress t of
      Just p -> Map.insertWith (++) (trip_id t) [(timestamp t, p)] m
      Nothing -> m
  )
  Map.empty
  ts
