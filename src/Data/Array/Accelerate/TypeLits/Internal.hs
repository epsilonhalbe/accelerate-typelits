{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Data.Array.Accelerate.TypeLits.Internal where

import           GHC.TypeLits ( Nat, KnownNat, natVal)

import           Control.Monad (replicateM)

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I
import           Data.Proxy (Proxy(..))
import           Data.Array.Accelerate ( (:.)((:.)), Array
                                       , Exp
                                       , DIM0, DIM1, DIM2, Z(Z)
                                       , Elt, Acc
                                       )

import           Test.SmallCheck.Series
import           Test.QuickCheck.Arbitrary

newtype AccScalar a = AccScalar { unScalar :: Acc (Array DIM0 a)}
                    deriving (Show)

-- | A typesafe way to represent an AccVector and its dimension
newtype AccVector (dim :: Nat) a = AccVector { unVector :: Acc (Array DIM1 a)}
                                 deriving (Show)

instance forall n a. (KnownNat n, Eq a, Elt a) => Eq (AccVector n a) where
    v == w = let v' = I.run $ unVector v
                 w' = I.run $ unVector w
              in A.toList v' == A.toList w'

instance forall mm n a. (Serial mm a, KnownNat n, Eq a, Elt a)
  => Serial mm (AccVector n a) where
      series = AccVector . A.use . A.fromList (Z:.n') <$> cons1 (replicate n')
        where n' = fromIntegral $ natVal (Proxy :: Proxy n)

instance forall n a. (KnownNat n, Arbitrary a, Eq a, Elt a)
  => Arbitrary (AccVector n a) where
      arbitrary = AccVector . A.use . A.fromList (Z:.n') <$> replicateM n' arbitrary
        where n' = fromIntegral $ natVal (Proxy :: Proxy n)

-- | A typesafe way to represent an AccMatrix and its rows/colums
newtype AccMatrix (rows :: Nat) (cols :: Nat) a = AccMatrix {unMatrix :: Acc (Array DIM2 a)}
                                                deriving (Show)

instance forall m n a. (KnownNat m, KnownNat n, Eq a, Elt a) => Eq (AccMatrix m n a) where
    v == w = let v' = I.run $ unMatrix v
                 w' = I.run $ unMatrix w
              in A.toList v' == A.toList w'

instance forall mm m n a. (Serial mm a, KnownNat m, KnownNat n, Eq a, Elt a)
  => Serial mm (AccMatrix m n a) where
      series = AccMatrix . A.use . A.fromList (Z:.m':.n') <$> cons1 (replicate $ m'*n')
        where m' = fromIntegral $ natVal (Proxy :: Proxy m)
              n' = fromIntegral $ natVal (Proxy :: Proxy n)

instance forall m n a. (KnownNat m, KnownNat n, Arbitrary a, Eq a, Elt a)
  => Arbitrary (AccMatrix m n a) where
      arbitrary = AccMatrix . A.use . A.fromList (Z:.m':.n') <$> replicateM (m'*n') arbitrary
        where m' = fromIntegral $ natVal (Proxy :: Proxy m)
              n' = fromIntegral $ natVal (Proxy :: Proxy n)

-- | a functor like instance for a functor like instance for Accelerate computations
-- instead of working with simple functions `(a -> b)` this uses (Exp a -> Exp b)
class AccFunctor f where
  afmap :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> f a -> f b

instance AccFunctor AccScalar  where
    afmap f (AccScalar a) = AccScalar (A.map f a)

instance forall n. (KnownNat n) => AccFunctor (AccVector n) where
    afmap f (AccVector a) = AccVector (A.map f a)

instance forall m n. (KnownNat m, KnownNat n) => AccFunctor (AccMatrix m n) where
    afmap f (AccMatrix a) = AccMatrix (A.map f a)

mkVector :: forall n a. (KnownNat n, Elt a) => [a] -> Maybe (AccVector n a)
-- | a smart constructor to generate Vectors - returning Nothing
-- if the input list is not as long as the dimension of the Vector
mkVector as = if length as == n'
                 then Just $ AccVector (A.use $ A.fromList (Z:.n') as)
                 else Nothing
  where n' = fromIntegral $ natVal (Proxy :: Proxy n)

mkMatrix :: forall m n a. (KnownNat m, KnownNat n, Elt a)
         => [a] -> Maybe (AccMatrix m n a)
-- | a smart constructor to generate Matrices - returning Nothing
-- if the input list is not as long as the "length" of the Matrix, i.e. rows*colums
mkMatrix as = if length as == m'*n'
                 then Just $ AccMatrix (A.use $ A.fromList (Z:. m':.n') as)
                 else Nothing
  where m' = fromIntegral $ natVal (Proxy :: Proxy m)
        n' = fromIntegral $ natVal (Proxy :: Proxy n)

mkScalar :: forall a. Elt a => Exp a -> AccScalar a
-- | a smart constructor to generate scalars
mkScalar = AccScalar . A.unit
