import Test.Tasty
import Test.Tasty.HUnit

import Client.Test
import DataStore.Test

main :: IO ()
main = defaultMain $ testGroup "bus-scrape tests" [
  Client.Test.clientTests
  ,DataStore.Test.dataStoreTests
  ]


