import Test.Tasty
import Test.Tasty.HUnit

import Client.Test

main :: IO ()
main = defaultMain Client.Test.clientTests


