module Main where

import Test.Tasty

import qualified Test.Data.Accelerate.TypeLits
import qualified Test.Data.Accelerate.TypeLits.Random


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
                 [ Test.Data.Accelerate.TypeLits.tests
                 , Test.Data.Accelerate.TypeLits.Random.tests
                 ]
