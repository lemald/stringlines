import Test.Tasty
import Test.Tasty.HUnit

import Client.Test
import Config.Test
import DataStore.Test
import Progress.Test

main :: IO ()
main = defaultMain $ testGroup "stringlines tests" [
  Client.Test.clientTests
  ,Config.Test.configTests
  ,DataStore.Test.dataStoreTests
  ,Progress.Test.progressTests
  ]


