{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as Ex
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Data.Sequence
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.Gnuplot.LineSpecification
import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.PNG
import Graphics.Gnuplot.Time
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Client
import DataStore
import TAPI

data Options = Options
  { optRouteID :: TAPI.RouteID
  , optDateStr :: String
  , optTzOffsetStr :: String
  , optOutFile :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { optRouteID = ""
  , optDateStr = ""
  , optTzOffsetStr = "-5"
  , optOutFile = "output.png"
  }

data Params = Params
  { paramRouteID :: TAPI.RouteID
  , paramDate :: Day
  , paramTzOffset :: Int
  , paramOutFile :: String
  }

options :: [OptDescr (Options -> Options)]
options =
  [Option ['r'] ["route"]
    (ReqArg (\r opts -> opts{ optRouteID = T.pack r }) "ROUTE")
    "route ID"
  ,Option ['d'] ["date"]
    (ReqArg (\d opts -> opts{ optDateStr = d }) "DATE")
    "date"
  ,Option ['z'] ["tzoffset"]
    (ReqArg (\z opts -> opts{ optTzOffsetStr = z }) "OFFSET")
    "time zone offset"
  ,Option ['o'] ["output"]
    (ReqArg (\o opts -> opts{ optOutFile = o }) "FILE")
    "output file"
  ]

main :: IO()
main = do
  argv <- getArgs
  params <- argsToParams argv
  case params of
    Left e -> do
      hPutStrLn stderr ("Error: " ++ e)
      exitWith $ ExitFailure 1
    Right p -> makeChartFromParams p

makeChartFromParams :: Params -> IO()
makeChartFromParams p = do
  con <- connectToDB
  results <- tripInfoByRouteForDay
             con
             (paramRouteID p)
             (paramDate p)
             (hoursToTimeZone $ paramTzOffset p)
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
    ,terminal $ Graphics.Gnuplot.Terminal.PNG.cons $ paramOutFile p
    ,Custom "terminal png size 15360,640" []
    ]
    paths
  closeDBCon con

argsToParams :: [String] -> IO(Either String Params)
argsToParams argv =
  case getOpt Permute options argv of
    (o, _, []  ) -> do
      let opts = foldr ($) defaultOptions o
      return
        (Params
         <$> case optRouteID opts of
               ""  -> Left "No route ID specified"
               rID -> Right rID
         <*> case optDateStr opts of
               "" -> Left "No date specified"
               d  -> Right $ read d
         <*> (Right $ read (optTzOffsetStr opts))
         <*> (Right $ optOutFile opts))
    (_, _, errs) -> ioError (userError
                             (concat errs ++ usageInfo header options))
  where header = "Usage: [OPTION...]"

-- TODO: These next two functions should live in their own files and
-- have tests
resultsToPaths :: [TripInfo] -> [(PlotStyle, [(Double, Double)])]
resultsToPaths ts =
  let paths = toList $ accumTripInfoMap ts
  in fmap (\d ->
             (PlotStyle{plotType = Lines
                       ,lineSpec = CustomStyle []},
              prepXTime d)
          ) $ fmap toList paths

accumTripInfoMap :: [TripInfo] -> Map.Map TAPI.TripID (Seq (UTCTime, Double))
accumTripInfoMap ts =
  foldl'
  (\m t ->
     case progress t of
      Just p -> Map.insertWith (><) (trip_id t) (singleton (timestamp t, p)) m
      Nothing -> m
  )
  Map.empty
  ts
