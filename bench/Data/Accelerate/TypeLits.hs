{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeLits
              (
              -- * Types
              Scalar,
              Vector,
              Matrix,
              -- * Classes
              AccFunctor(..),
              -- * Constructors
              mkMatrix,
              mkVector,
              identityMatrix,
              -- * Functions
              -- ** Scalar & X
              (.*^),
              (./^),
              (.*#),
              (./#),
              -- ** Matrix & Vector
              (#*^),
              (^*#),
              -- ** Vector & Vector
              (^+^),
              (^-^),
              (^*^),
              -- ** Matrix & Matrix
              (#+#),
              (#-#),
              (#*#),
              (#**.),
              )
              where

import qualified Data.Array.Accelerate as A

import GHC.TypeLits
import Data.Array.Accelerate ( (:.)(..), Array
                             , Exp
                             , DIM0, DIM1, DIM2, Z(Z)
                             , IsFloating, IsNum, Elt, Acc
                             , All(All)
                             )

import           Data.Proxy
type Scalar a = Acc (Array DIM0 a)

-- | A typesafe way to represent a Vector and its dimension
newtype Vector (dim :: Nat) a = AccVector { unVector :: Acc (Array DIM1 a)}

-- | A typesafe way to represent a Matrix and its rows/colums
newtype Matrix (rows :: Nat) (cols :: Nat) a = AccMatrix {unMatrix :: Acc (Array DIM2 a)}

-- | a functor like instance for a functor like instance for Accelerate computations
-- instead of working with simple functions `(a -> b)` this uses (Exp a -> Exp b)
class AccFunctor f where
  afmap :: forall a b. (Elt a, Elt b) => (Exp a -> Exp b) -> f a -> f b

instance forall n. (KnownNat n) => AccFunctor (Vector n) where
    afmap f (AccVector a) = AccVector (A.map f a)

instance forall m n. (KnownNat m, KnownNat n) => AccFunctor (Matrix m n) where
    afmap f (AccMatrix a) = AccMatrix (A.map f a)

mkVector :: forall n a. (KnownNat n, Elt a) => [a] -> Maybe (Vector n a)
-- | a smart constructor to generate Vectors - returning Nothing
-- if the input list is not as long as the dimension of the Vector
mkVector as = if length as == n'
                 then Just $ AccVector (A.use $ A.fromList (Z:.n') as)
                 else Nothing
  where n' = fromInteger $ natVal (Proxy :: Proxy n)

mkMatrix :: forall m n a. (KnownNat m, KnownNat n, Elt a)
         => [a] -> Maybe (Matrix m n a)
-- | a smart constructor to generate Matrices - returning Nothing
-- if the input list is not as long as the "length" of the Matrix, i.e. rows*colums
mkMatrix as = if length as == m'*n'
                 then Just $ AccMatrix (A.use $ A.fromList (Z:. m':.n') as)
                 else Nothing
  where m' = fromInteger $ natVal (Proxy :: Proxy m)
        n' = fromInteger $ natVal (Proxy :: Proxy n)

(#*^) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => Matrix m n a -> Vector n a -> Vector n a
-- | the usual matrix-vector product
--
-- > ⎛ w₁₁ w₁₂ … w₁ₙ ⎞   ⎛x₁⎞   ⎛ w₁₁*x₁ + w₁₂*x₂ + … w₁ₙ*xₙ ⎞
-- > ⎜ w₂₁ w₂₂ … w₂ₙ ⎟   ⎜x₂⎟   ⎜ w₂₁*x₁ + w₂₂*x₂ + … w₂ₙ*xₙ ⎟
-- > ⎜  .   .     .  ⎟   ⎜. ⎟   ⎜  .          .          .   ⎟
-- > ⎜  .   .     .  ⎟ ✕ ⎜. ⎟ = ⎜  .          .          .   ⎟
-- > ⎜  .   .     .  ⎟   ⎜. ⎟   ⎜  .          .          .   ⎟
-- > ⎜  .   .     .  ⎟   ⎜. ⎟   ⎜  .          .          .   ⎟
-- > ⎝ wₘ₁ wₘ₂ … wₘₙ ⎠   ⎝xₙ⎠   ⎝ wₘ₁*x₁ + wₘ₂*x₂ + … wₘₙ*xₙ ⎠

ma #*^ va = let ma' = unMatrix ma
                va' = unVector va
            in AccVector $ A.fold1 (+)
                         $ A.zipWith (*)
                                    ma'
                                    (A.replicate (A.lift $ Z :. m' :. All) va')
  where m'  = fromInteger $ natVal (Proxy :: Proxy m) :: Int

infixl 7 #*^

(^*#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => Vector m a -> Matrix m n a -> Vector n a
-- | the usual matrix-vector product
--
-- > ⎛x₁⎞T  ⎛w₁₁ w₁₂ … w₁ₙ ⎞    ⎛ x₁*w₁₁ + x₂*w₁₂ + … xₙ*w₁ₙ ⎞
-- > ⎜x₂⎟   ⎜w₂₁ w₂₂ … w₂ₙ ⎟    ⎜ x₁*w₂₁ + x₂*w₂₂ + … xₙ*w₂ₙ ⎟
-- > ⎜. ⎟   ⎜ .   .     .  ⎟    ⎜  .         .           .   ⎟
-- > ⎜. ⎟ ✕ ⎜ .   .     .  ⎟ =  ⎜  .         .           .   ⎟
-- > ⎜. ⎟   ⎜ .   .     .  ⎟    ⎜  .         .           .   ⎟
-- > ⎜. ⎟   ⎜ .   .     .  ⎟    ⎜  .         .           .   ⎟
-- > ⎝xₘ⎠   ⎝wₘ₁ wₘ₂ … wₘₙ ⎠    ⎝ x₁*wₘ₁ + x₂*wₘ₂ + … xₙ*wₘₙ ⎠
va ^*# ma = let va' = unVector va
                ma' = unMatrix ma
            in AccVector $ A.fold1 (+)
                         $ A.zipWith (*)
                                    (A.replicate (A.lift $ Z :. n' :. All) va')
                                    ma'
  where n'  = fromInteger $ natVal (Proxy :: Proxy n) :: Int

infixr 7 ^*#

(^+^), (^-^) :: forall n a. (KnownNat n, IsNum a, Elt a)
             => Vector n a -> Vector n a -> Vector n a
-- | the usual vector addition/subtraction
--
-- > ⎛v₁⎞   ⎛w₁⎞   ⎛ v₁+w₁ ⎞
-- > ⎜v₂⎟   ⎜w₂⎟   ⎜ v₂+w₁ ⎟
-- > ⎜. ⎟   ⎜. ⎟   ⎜   .   ⎟
-- > ⎜. ⎟ + ⎜. ⎟ = ⎜   .   ⎟
-- > ⎜. ⎟   ⎜. ⎟   ⎜   .   ⎟
-- > ⎜. ⎟   ⎜. ⎟   ⎜   .   ⎟
-- > ⎝vₙ⎠   ⎝wₙ⎠   ⎝ vₙ*wₙ ⎠

v ^+^ w = AccVector $ A.zipWith (+) (unVector v) (unVector w)
v ^-^ w = AccVector $ A.zipWith (-) (unVector v) (unVector w)

infixl 6 ^+^
infixl 6 ^-^

(^*^) :: forall n a. (KnownNat n, IsNum a, Elt a)
      => Vector n a -> Vector n a -> Scalar a
-- | the usual vector addition/subtraction
--
-- > ⎛v₁⎞   ⎛w₁⎞
-- > ⎜v₂⎟   ⎜w₂⎟
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎜. ⎟ * ⎜. ⎟ = v₁*w₁ + v₂*w₁ + … + vₙ*wₙ
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎝vₙ⎠   ⎝wₙ⎠

v ^*^ w = A.sum $ A.zipWith (*) (unVector v) (unVector w)

infixl 7 ^*^

(#+#), (#-#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
             => Matrix m n a -> Matrix m n a -> Matrix m n a
-- | the usual vector addition/subtraction
--
-- > ⎛ v₁₁ v₁₂ … v₁ₙ ⎞     ⎛ w₁₁ w₁₂ … w₁ₙ ⎞     ⎛ v₁₁+w₁₁ v₁₂+w₁₂ … v₁ₙ+w₁ₙ ⎞
-- > ⎜ v₂₁ v₂₂ … v₂ₙ ⎟     ⎜ w₂₁ w₂₂ … w₂ₙ ⎟     ⎜ v₂₁+w₂₁ v₂₂+w₂₂ … v₂ₙ+w₂ₙ ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜    .       .         .    ⎟
-- > ⎜  .   .     .  ⎟  +  ⎜  .   .     .  ⎟  =  ⎜    .       .         .    ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜    .       .         .    ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜    .       .         .    ⎟
-- > ⎝ vₘ₁ vₘ₂ … vₘₙ ⎠     ⎝ wₘ₁ wₘ₂ … wₘₙ ⎠     ⎝ vₘ₁+wₘ₁ wₘ₂+vₘ₂ … vₘₙ+wₘₙ ⎠

v #+# w = AccMatrix $ A.zipWith (+) (unMatrix v) (unMatrix w)
v #-# w = AccMatrix $ A.zipWith (-) (unMatrix v) (unMatrix w)

infixl 6 #+#
infixl 6 #-#

(#*#) :: forall k m n a. (KnownNat k, KnownNat m, KnownNat n, IsNum a, Elt a)
      => Matrix k m a -> Matrix m n a -> Matrix k n a
-- | the usual vector addition/subtraction
--
-- > ⎛ v₁₁ v₁₂ … v₁ₘ ⎞     ⎛ w₁₁ w₁₂ … w₁ₙ ⎞     ⎛ (v₁₁*w₁₁+v₁₂*w₂₁+…+v₁ₘ*wₘ₁) . . . (v₁₁*w₁ₙ+v₁₂*w₂ₙ+…+v₁ₘ*wₘₙ) ⎞
-- > ⎜ v₂₁ v₂₂ … v₂ₘ ⎟     ⎜ w₂₁ w₂₂ … w₂ₙ ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟  *  ⎜  .   .     .  ⎟  =  ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎝ vₖ₁ vₖ₂ … vₖₘ ⎠     ⎝ wₘ₁ wₘ₂ … wₘₙ ⎠     ⎝ (vₖ₁*w₁₁+vₖ₂*w₂₁+…+vₖₘ*wₘ₁) . . . (vₖ₁*w₁ₙ+vₖ₂*w₂ₙ+…+vₖₘ*wₘₙ) ⎠

v #*# w = AccMatrix $ A.generate (A.index2 k' n') (aux v w)
  where k' = fromInteger $ natVal (Proxy :: Proxy k)
        n' = fromInteger $ natVal (Proxy :: Proxy n)
        aux a b sh = let (Z:.i:.j) = A.unlift sh :: (Z:.Exp Int):.Exp Int
                         a' = A.slice (unMatrix a) (A.lift $ Z:.i:.All)
                         b' = A.slice (unMatrix b) (A.lift $ Z:.All:.j)
                      in A.the $ A.sum $ A.zipWith (*) a' b'


infixl 7 #*#

(.*^) :: forall n a. (KnownNat n, IsNum a, Elt a)
      => Exp a -> Vector n a -> Vector n a
-- | the usual matrix-vector product
--
-- >     ⎛x₁⎞   ⎛ a*x₁ ⎞
-- >     ⎜x₂⎟   ⎜ a*x₂ ⎟
-- >     ⎜. ⎟   ⎜  .   ⎟
-- > a • ⎜. ⎟ = ⎜  .   ⎟
-- >     ⎜. ⎟   ⎜  .   ⎟
-- >     ⎜. ⎟   ⎜  .   ⎟
-- >     ⎝xₙ⎠   ⎝ a*xₙ ⎠

a .*^ v = let v' = unVector v
          in AccVector $ A.map (* a) v'

(./^) :: forall n a. (KnownNat n, IsFloating a, Elt a)
      => Exp a -> Vector n a -> Vector n a
a ./^ v = let v' = unVector v
          in AccVector $ A.map (/ a) v'

infixl 7 .*^
infixl 7 ./^

(.*#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => Exp a -> Matrix m n a -> Matrix m n a
-- | the usual matrix-vector product
--
-- >     ⎛ w₁₁ w₁₂ … w₁ₙ ⎞    ⎛ a*w₁₁ a*w₁₂ … a*w₁ₙ ⎞
-- >     ⎜ w₂₁ w₂₂ … w₂ₙ ⎟    ⎜ a*w₂₁ a*w₂₂ … a*w₂ₙ ⎟
-- >     ⎜  .   .     .  ⎟    ⎜  .      .      .    ⎟
-- > a • ⎜  .   .     .  ⎟ =  ⎜  .      .      .    ⎟
-- >     ⎜  .   .     .  ⎟    ⎜  .      .      .    ⎟
-- >     ⎜  .   .     .  ⎟    ⎜  .      .      .    ⎟
-- >     ⎝ wₘ₁ wₘ₂ … wₘₙ ⎠    ⎝ a*wₘ₁ a*wₘ₂ … a*wₘₙ ⎠

a .*# v = let v' = unMatrix v
          in AccMatrix $ A.map (* a) v'

(./#) :: forall m n a. (KnownNat m ,KnownNat n, IsFloating a, Elt a)
      => Exp a -> Matrix m n a -> Matrix m n a
a ./# v = let v' = unMatrix v
          in AccMatrix $ A.map (/ a) v'

infixl 7 .*#
infixl 7 ./#

(#**.) :: forall n a. (KnownNat n, IsNum a, Elt a)
       => Matrix n n a -> Int -> Matrix n n a
_ #**. 0 = identityMatrix
v #**. 1 = v
v #**. i | i < 0 = error $ "no negative exponents allowed in matrix exponetiation,"
                        ++ "inverse function not yet implemented"
         | otherwise = (v#**. (i-1)) #*# v

infixr 8 #**.

identityMatrix :: forall n a. (KnownNat n, IsNum a, Elt a) => Matrix n n a
identityMatrix = AccMatrix $ A.use $ A.fromFunction (Z:.n':.n') aux
  where aux :: DIM2 -> a
        aux (Z:.i:.j) = if i == j then 1 else 0
        n' = fromInteger $ natVal (Proxy :: Proxy n)

