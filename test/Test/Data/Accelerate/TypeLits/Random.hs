{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Test.Data.Accelerate.TypeLits.Random where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
  [--scprop
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [--hunit
  ]
