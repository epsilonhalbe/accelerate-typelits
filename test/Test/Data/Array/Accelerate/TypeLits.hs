{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Test.Data.Array.Accelerate.TypeLits where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Array.Accelerate.Interpreter (run)
import Data.Array.Accelerate.TypeLits

import GHC.TypeLits

tests :: TestTree
tests = testGroup "Data.Array.Accelerate.TypeLits" [properties, unitTests]

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
  [ testCase "identity" $
      j4 @?= m4
  , testCase "m #*# m" $
      i4 #*# i4 @?= i4
  , testCase "m #*^ v" $
      fmap (i4 #*^) v4 @?= v4
  --hunit
  ]

i4 :: AccMatrix 4 4 Int
i4 = identityMatrix

j4 :: Maybe (AccMatrix 4 4 Int)
j4 = Just identityMatrix

v4 :: Maybe (AccVector 4 Int)
v4 = mkVector [1,1,1,1]

m4 :: Maybe (AccMatrix 4 4 Int)
m4 = mkMatrix [1,0,0,0 ,0,1,0,0 ,0,0,1,0 ,0,0,0,1]
