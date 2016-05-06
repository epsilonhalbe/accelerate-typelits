module Main where

import Test.Tasty

import qualified Test.Data.Array.Accelerate.TypeLits
import qualified Test.Data.Array.Accelerate.TypeLits.Random


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
                 [ Test.Data.Array.Accelerate.TypeLits.tests
                 ]
