{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Progress.Test where

import Data.Geo.Jord.LatLong
import Test.Tasty
import Test.Tasty.HUnit

import Progress
import TAPI

progressTests :: TestTree
progressTests = testGroup "Progress" $
  [testCase "closestPointAlongRoute for a point far away from the route" $
    closestPointAlongRoute farAwayPoint (shapeToPoints shape) @?= Nothing
  ,testCase "closestPointAlongRoute for a point close to the route" $
    closestPointAlongRoute closePoint (shapeToPoints shape) @?= Just pointAlongRoute
  ,testCase "progressOnRoute for vehicle at the end of the route" $
    progressOnRoute busAtArlingtonHts shape @?= Just 1.0
  ,testCase "progressOnRoute for vehicle at the beginning of the route" $
    progressOnRoute busAtHarvard shape @?= Just 0.0
  ]

shape :: Shape
shape = Shape{
  polyline = "gfsaGvlaqL[Ek@Ck@A[?]BU?sAHg@DWDODKJMPM\\KNEQAOU`@c@`@EBC@[PeCn@g@HM@??[@cBG_BKmBIq@Eq@GyAGoAGcDQ??OAiDQoFYcCO_AEM?OA??yAIyAIeBK}AK_ACy@FiAPu@N??YFa@FSDaARw@\\UL[T??WT_B`Bm@r@e@n@Sb@_@|@gAxCq@hBIT]fACDGL??cAhBc@j@k@n@m@v@MNg@j@WX??q@v@oBxBiArA}@fAc@f@??STSTwAbBq@t@eAnAaAjA]b@qAxAMNuA~As@z@??MPsB~B{@jAgBbCgA~Ag@x@mA`C??IR[x@}@pCw@bCGP??y@vBGf@O\\}@tBSV??_AtAkAtAY`@mAzAMP??IJu@~@i@r@Y^cAtAa@h@w@fAe@n@KN??e@l@m@x@g@r@}@nAY\\w@hAg@p@OR??kBjCeB`CiA|AuAlBQV??Y^{AvBy@hAY`@w@fA??UZ]`@[\\k@b@u@^_ChAwBfAQH??uAr@{@b@uAv@yAz@a@`@??CBWXiApAaApAo@t@kAdBMX??a@~@[`AEPe@pAc@`BIZQr@??Sv@_@zAOfAAZAl@S`C]xECh@????]lEShCOjBMjB?H??UhCWfDOlBYxDMlAIf@Kd@s@lCg@zAEN??Uj@sAjDi@xAy@vBKRO`@??IT{@nBO^u@bBiAlCw@nBYh@??KRq@jBwA`DsBzE[b@KP??}@tAORc@l@o@~@uApBS`@IRMl@SnCEf@??AV}@`FQ`AOv@MhAEp@CvA?~AAl@???VCtBC|BC~CEbEAf@??Ad@[tDOpBAJaAPGdA"
  ,name = "Arlington Heights"
  ,direction_id = 0
  }

farAwayPoint :: LatLong
farAwayPoint = decimalLatLong 42.405447 (-71.136629)

closePoint :: LatLong
closePoint = decimalLatLong 42.403261 (-71.138979)

pointAlongRoute :: LatLong
pointAlongRoute = decimalLatLong 42.40324 (-71.13908)

busAtArlingtonHts :: Vehicle
busAtArlingtonHts = Vehicle{
  current_status = "foo"
  ,current_stop_sequence = 10
  ,speed = 0.0
  ,direction_id = 0
  ,bearing = Just 180
  ,label = "Arlington Heights"
  ,latitude = 42.424736
  ,longitude = (-71.185059)
  ,updated_at = read "2019-01-01 12:00:00"
  }

busAtHarvard :: Vehicle
busAtHarvard = Vehicle{
  current_status = "foo"
  ,current_stop_sequence = 10
  ,speed = 0.0
  ,direction_id = 0
  ,bearing = Just 180
  ,label = "Arlington Heights"
  ,latitude = 42.374074
  ,longitude = (-71.118839)
  ,updated_at = read "2019-01-01 12:00:00"
  }
