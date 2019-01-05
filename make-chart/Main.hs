{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
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
  plotPathsStyle
    [Key Nothing
    ,XLabel "Time"
    ,XTime
    ,YLabel "Progress along route"
    ,YRange (0, 1)
    -- TODO: Output file should also come from args
    ,terminal $ Graphics.Gnuplot.Terminal.PNG.cons "output.png"
    ]
    (resultsToPaths results)

resultsToPaths :: [TripInfo] -> [(PlotStyle, [(Double, Double)])]
resultsToPaths ts = []
