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
import Text.Read

import Client
import DataStore
import TAPI

data Options = Options
  { optRouteID :: TAPI.RouteID
  , optDateStr :: String
  , optTzOffsetStr :: String
  , optOutFile :: String
  , optDBFile :: String
  , optDir0 :: Bool
  , optDir1 :: Bool
  }

defaultOptions :: Options
defaultOptions = Options
  { optRouteID = ""
  , optDateStr = ""
  , optTzOffsetStr = "-5"
  , optOutFile = "output.png"
  , optDBFile = "locations.db"
  , optDir0 = False
  , optDir1 = False
  }

data Params = Params
  { paramRouteID :: TAPI.RouteID
  , paramDate :: Day
  , paramTzOffset :: Int
  , paramOutFile :: String
  , paramDBFile :: String
  , paramDir0 :: Bool
  , paramDir1 :: Bool
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
  -- TODO: Make this option actually do something
  -- ,Option ['b'] ["database"]
  --   (ReqArg (\b opts -> opts{ optDBFile = b }) "FILE")
  --   "database file"
  ,Option [] ["direction-0"]
    (NoArg (\opts -> opts{ optDir0 = True }))
    "include direction ID 0"
  ,Option [] ["direction-1"]
    (NoArg (\opts -> opts{ optDir1 = True }))
    "include direction ID 1"
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
             ([0 | paramDir0 p] ++ [1 | paramDir1 p])
             (paramDate p)
             (hoursToTimeZone $ paramTzOffset p)
  let paths = resultsToPaths results
  plotPathsStyle
    [Key Nothing
    ,XLabel "Time (UTC)"
    ,XTime
    ,XTicks $ Just ["600", "offset 0,graph 0.015"]
    ,YLabel "Progress along route"
    ,YRange (0, 1)
    ,Grid $ Just ["xtics", "ytics"]
    ,terminal $ Graphics.Gnuplot.Terminal.PNG.cons $ paramOutFile p
    ,Custom "terminal png size 15360,640" []
    ]
    paths
  closeDBCon con

argsToParams :: [String] -> IO(Either String Params)
argsToParams argv =
  case getOpt Permute options argv of
    (o, _, []) -> do
      let opts = foldl (flip Prelude.id) defaultOptions o
      return
        (Params
         <$> case optRouteID opts of
               ""  -> Left "No route ID specified"
               rID -> Right rID
         <*> case optDateStr opts of
               "" -> Left "No date specified"
               d  -> case readMaybe d of
                       Nothing -> Left "Invalid date"
                       Just d  -> Right d
         <*> case readMaybe (optTzOffsetStr opts) of
               Nothing -> Left "Invalid time zone offset"
               Just o  -> Right o
         <*> Right (optOutFile opts)
         <*> Right (optDBFile opts)
         <*> Right ((not (optDir0 opts) || optDir1 opts) || optDir0 opts)
         <*> Right ((not (optDir0 opts) || optDir1 opts) || optDir1 opts))
    (_, _, errs) -> ioError (userError
                             (concat errs ++ usageInfo header options))
  where header = "Usage: [OPTION...]"

-- TODO: These next two functions should live in their own files and
-- have tests
resultsToPaths :: [TripInfo] -> [(PlotStyle, [(Double, Double)])]
resultsToPaths ts =
  let paths = toList $ accumTripInfoMap ts
  in fmap ((\d ->
              (PlotStyle{plotType = Lines
                        ,lineSpec = CustomStyle []},
               prepXTime d)
           ) . toList) paths

accumTripInfoMap :: [TripInfo] -> Map.Map TAPI.VehicleID (Seq (UTCTime, Double))
accumTripInfoMap =
  foldl'
  (\m t ->
     case progress t of
      Just p -> Map.insertWith (><) (vehicle_id t) (singleton (timestamp t, p)) m
      Nothing -> m
  )
  Map.empty
