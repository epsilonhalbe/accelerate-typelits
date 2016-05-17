module Main where

import Test.Tasty

import qualified Test.Data.Array.Accelerate.TypeLits
import qualified Test.Data.Array.Accelerate.TypeLits.Internal
import qualified Test.Data.Array.Accelerate.TypeLits.System.Random.MWC


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
                 [ Test.Data.Array.Accelerate.TypeLits.tests
                 , Test.Data.Array.Accelerate.TypeLits.Internal.tests
                 , Test.Data.Array.Accelerate.TypeLits.System.Random.MWC.tests
                 ]
