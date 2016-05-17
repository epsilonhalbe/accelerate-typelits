{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Test.Data.Array.Accelerate.TypeLits where

import           Test.Tasty
import           Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit
import qualified Test.HUnitPlus.Base as H
import qualified Test.SmallCheck.Series as SC

import Data.Array.Accelerate.TypeLits

import Data.Maybe (isJust, isNothing)
import Control.Exception

tests :: TestTree
tests = testGroup "Data.Array.Accelerate.TypeLits" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
 [ testGroup   "* Functions"
   [ testGroup "** Classes"
     [ testGroup "AccFunctor"
       [ SC.testProperty "Vector: afmap id = id" $
           \v -> afmap id (v :: AccVector 3 Int) == v
       , SC.testProperty "Matrix: afmap id = id" $
           \ma -> afmap id (ma :: AccMatrix 3 3 Int) == ma
       --scprop
       ]
     ]
   , testGroup "** Scalar & X"
     [ testGroup ".*^"
       [ SC.testProperty "universal property of zero" $
           \v -> 0 .*^ v == (zeroV                           :: AccVector 2 Int)
       , SC.testProperty "universal property of 1" $
           \v -> 1 .*^ v == (v                               :: AccVector 2 Int)
       , SC.testProperty "universal property of 2" $
           \v -> 2 .*^ v == v ^+^ (v                         :: AccVector 2 Int)
       --scprop
       ]
     , testGroup "./^"
       [ SC.testProperty "universal property of 1" $
           \v -> 1 ./^ v == (v                               :: AccVector 2 Float)
       --scprop
       ]
     , testGroup ".*#"
       [ SC.testProperty "universal property of zero" $
           \ma -> 0 .*# ma == (zeroM                       :: AccMatrix 3 2 Int)
       , SC.testProperty "universal property of 1" $
           \ma -> 1 .*# ma == (ma                          :: AccMatrix 3 2 Int)
       , SC.testProperty "universal property of 2" $
           \ma -> 2 .*# ma == ma #+# (ma                   :: AccMatrix 3 2 Int)
       --scprop
       ]
     , testGroup "./#"
       [ SC.testProperty "universal property of 1" $
           \ma -> 1 ./# ma == (ma                          :: AccMatrix 3 2 Float)
       --scprop
       ]
     ]
   , testGroup "** AccMatrix & AccVector"
     [ testGroup "#*^"
       [ SC.testProperty "id . v == v" $
           \v -> identityMatrix #*^ v == (v                  :: AccVector 2 Int)
       , SC.testProperty "0 . v == 0" $
           \v -> (zeroM :: AccMatrix 2 2 Int) #*^ v == zeroV
       , SC.testProperty "ma . 0 == 0" $
           \ma -> (ma :: AccMatrix 2 2 Int) #*^ zeroV == zeroV
       --scprop
       ]
     , testGroup "^*#"
       [ SC.testProperty "v . id == v" $
           \v -> v ^*# identityMatrix == (v                  :: AccVector 2 Int)
       , SC.testProperty "v . 0 == 0" $
           \v -> v ^*# (zeroM :: AccMatrix 2 2 Int) == zeroV
       , SC.testProperty "ma . 0 == 0" $
           \ma -> zeroV ^*# (ma :: AccMatrix 2 2 Int) == zeroV
       --scprop
       ]
     ]
   , testGroup "** AccVector & AccVector"
     [ testGroup "^+^"
       [ SC.testProperty "0 + v = v" $
           \v -> zeroV ^+^ v ==  (v                          :: AccVector 2 Int)
       , SC.testProperty "v + 0 = v" $
           \v -> zeroV ^+^ v ==  (v                          :: AccVector 2 Int)
       , SC.testProperty "v + (-v) = 0 with operator precedence" $
           \v -> v ^+^ (-1) .*^ v == (zeroV                  :: AccVector 2 Int)
       , SC.testProperty "(-v) + v = 0 with operator precedence" $
           \v -> (-1) .*^ v ^+^ v == (zeroV                  :: AccVector 2 Int)
       , SC.testProperty "commutativity: v + w = w + v" $
           \v w -> v ^+^ w == w ^+^ (v                       :: AccVector 2 Int)
       , SC.testProperty "associativity: (u + v) + w = u + (v + w)" $
           \u v w -> (u ^+^ v) ^+^ w == (u ^+^ (v ^+^ w)     :: AccVector 2 Int)
       --scprop
       ]
     , testGroup "^-^"
       [ SC.testProperty "v - v = 0" $
           \v -> v ^-^ v == (zeroV                           :: AccVector 2 Int)
       ]
     , testGroup "^*^"
       [ SC.testProperty "zeroV is orthogonal for everything" $
           \v -> zeroV ^*^ (v :: AccVector 2 Int) == mkScalar 0
       , SC.testProperty "zeroV is orthogonal for everything" $
           \v -> (v :: AccVector 2 Int) ^*^ zeroV == mkScalar 0
       , SC.testProperty "inner product is commutative" $
           \v w -> v ^*^ w == w ^*^ (v                       :: AccVector 2 Int)
       --scprop
       ]
     ]
   , testGroup "** AccMatrix & AccMatrix"
     [ testGroup "#+#"
       [ SC.testProperty "0 + ma = ma" $
           \ma -> zeroM #+# ma ==  (ma                                         :: AccMatrix 3 2 Int)
       , SC.testProperty "ma + 0 = ma" $
           \ma -> zeroM #+# ma ==  (ma                                         :: AccMatrix 3 2 Int)
       , SC.testProperty "ma + (-ma) = 0 with operator precedence" $
           \ma -> ma #+# (-1) .*# ma == (zeroM                                 :: AccMatrix 3 2 Int)
       , SC.testProperty "(-ma) + ma = 0 with operator precedence" $
           \ma -> (-1) .*# ma #+# ma == (zeroM                                 :: AccMatrix 3 2 Int)
       , SC.testProperty "commutativity: ma + -mb = -mb + ma" $
           \ma mb -> ma #+# mb == mb #+# (ma                                   :: AccMatrix 3 2 Int)
       , SC.testProperty "associativity: (mc + ma) + -mb = mc + (ma + -mb)" $
           \ma mb mc -> (ma #+# mb) #+# mc == (ma #+# (mb #+# mc)              :: AccMatrix 3 2 Int)
       --scprop
       ]
     , testGroup "#-#"
       [ SC.testProperty "ma - ma = 0" $
           \ma -> ma #-# ma == (zeroM                                          :: AccMatrix 3 2 Int)
       ]
     , testGroup "#*#"
       [ SC.testProperty "id * ma = ma" $
           \ma -> identityMatrix #*# ma == (ma                                 :: AccMatrix 3 2 Int)
       , SC.testProperty "ma * id = ma" $
           \ma -> ma #*# identityMatrix == (ma                                 :: AccMatrix 3 2 Int)
       --scprop
       ]
     , testGroup "#**."
       [ SC.testProperty "id^n = id" $
           \n -> let n' = SC.getNonNegative n
                 in identityMatrix #**. n' == (identityMatrix      :: AccMatrix 2 2 Int)
       , SC.testProperty "exponential law: a^n * a^m = a^(n+m)" $
           \n m ma -> let m' = SC.getNonNegative m
                          n' = SC.getNonNegative n
                       in (ma #**. m') #*# (ma #**. n') == (ma #**. (m' +n')   :: AccMatrix 2 2 Int)
       , SC.testProperty "exponentiation of diagonal matrices" $
           \k l m n -> let n' = SC.getNonNegative n
                        in unsafeMkMatrix [k,0,0
                                          ,0,l,0
                                          ,0,0,m] #**. n'
                          == (unsafeMkMatrix [k^n', 0  , 0
                                             , 0  ,l^n', 0
                                             , 0  , 0  ,m^n']                  :: AccMatrix 3 3 Int)
       --scprop
       ]
     ]
   , testGroup "** Utility functions"
     [ testGroup "transpose"
       [ SC.testProperty "transpose . transpose = id" $
           \ma -> transpose (transpose ma) == (ma                              :: AccMatrix 3 2 Int)
       --scprop
       ]
     , testGroup "zipWithV"
       [ SC.testProperty "zipWithV const v w = v" $
           \v w -> zipWithV const (v :: AccVector 3 Int) (w :: AccVector 3 Int) == v
       --scprop
       ]
     , testGroup "zipWithM"
       [ SC.testProperty "zipWithM const ma mb = ma" $
           \ma mb -> zipWithM const (ma :: AccMatrix 3 2 Int) (mb :: AccMatrix 3 2 Int) == ma
       --scprop
       ]
     ]
   ]
 ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [--qcprop
  ]

unitTests :: TestTree
unitTests = testGroup "HUnit tests"
 [ testGroup   "* Constructors"
   [ testGroup "identity"
     [ testCase "dim 2" $
         unsafeMkMatrix [1,0
                        ,0,1]
         @=? (identityMatrix :: AccMatrix 2 2 Int)
     , testCase "dim 3" $
         unsafeMkMatrix [1,0,0
                        ,0,1,0
                        ,0,0,1]
         @=? (identityMatrix :: AccMatrix 3 3 Int)
     , testCase "dim 4" $
         unsafeMkMatrix [1,0,0,0
                        ,0,1,0,0
                        ,0,0,1,0
                        ,0,0,0,1]
         @=? (identityMatrix :: AccMatrix 4 4 Int)
     --hunit
     ]
   , testGroup "mkMatrix"
     [ testCase "dim4 - ok" $
         isJust (mkMatrix [1..16] :: Maybe (AccMatrix 4 4 Int)) @=? True
     , testCase "dim4 - fail - too short" $
         isNothing (mkMatrix [1..15] :: Maybe (AccMatrix 4 4 Int)) @=? True
     , testCase "dim4 - fail - too long" $
         isNothing (mkMatrix [1..17] :: Maybe (AccMatrix 4 4 Int)) @=? True
     --hunit
     ]
   , testGroup "mkVector"
     [ testCase "dim4 - ok" $
         isJust (mkVector [1..4] :: Maybe (AccVector 4 Int)) @=? True
     , testCase "dim4 - fail - too short" $
         isNothing (mkVector [1..3] :: Maybe (AccVector 4 Int)) @=? True
     , testCase "dim4 - fail - too long" $
         isNothing (mkVector [1..5] :: Maybe (AccVector 4 Int)) @=? True
     --hunit
     ]
   ]
 , testGroup   "* Functions"
   [ testGroup "** Scalar & X"
     [ testGroup ".*^"
       [ testCase "2 .*^ <1,2>" $
           2 .*^ (unsafeMkVector [1,2] :: AccVector 2 Int) @?= (unsafeMkVector [2,4] :: AccVector 2 Int)
       , testCase "2 .*^ <1,2>" $
           (-2) .*^ (unsafeMkVector [1,2] :: AccVector 2 Int) @?= (unsafeMkVector [-2,-4] :: AccVector 2 Int)
       --hunit
       ]
     , testGroup "./^"
       [--hunit
       ]
     , testGroup ".*#"
       [--hunit
       ]
     , testGroup "./#"
       [--hunit
       ]
     ]
   , testGroup "** AccMatrix & AccVector"
     [ testGroup "#*^"
       [ testCase "m #*^ v" $
          fmap (i4 #*^) v4 @?= v4
       --hunit
       ]
     , testGroup "^*#"
       [--hunit
       ]
     ]
   , testGroup "** AccVector & AccVector"
     [ testGroup "^+^"
       [--hunit
       ]
     , testGroup "^-^"
       [--hunit
       ]
     , testGroup "^*^"
       [--hunit
       ]
     ]
   , testGroup "** AccMatrix & AccMatrix"
     [ testGroup "#+#"
       [--hunit
       ]
     , testGroup "#-#"
       [--hunit
       ]
     , testGroup "#*#"
       [ testCase "i4 #*# i4 == i4 " $
           i4 #*# i4 @?= i4
       , testCase "a #*# i4 == i4 " $
           i4 #*# i4 @?= i4
       --hunit
       ]
     , testGroup "#**."
       [testCase "id ^ (-1) = fail" $
           H.assertThrowsExact (ErrorCall "")
            (return (identityMatrix #**. (-1)) :: IO (AccMatrix 3 3 Int))
       ]
     ]
   , testGroup "** Utility functions"
     [ testGroup "transpose"
       [--hunit
       ]
     , testGroup "zipWithV"
       [--hunit
       ]
     , testGroup "zipWithM"
       [--hunit
       ]
     ]
   ]
 ]
i4 :: AccMatrix 4 4 Int
i4 = identityMatrix

j4 :: Maybe (AccMatrix 4 4 Int)
j4 = Just identityMatrix

v4 :: Maybe (AccVector 4 Int)
v4 = mkVector [1,1,1,1]

m4 :: Maybe (AccMatrix 4 4 Int)
m4 = mkMatrix [1,0,0,0 ,0,1,0,0 ,0,0,1,0 ,0,0,0,1]
