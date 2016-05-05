{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Data.Accelerate.TypeLits
              (
              -- * Types
              AccScalar,
              AccVector,
              AccMatrix,
              -- * Classes
              AccFunctor(..),
              -- * Constructors
              mkMatrix,
              mkVector,
              mkScalar,
              unMatrix,
              unVector,
              unScalar,
              identityMatrix,
              -- * Functions
              -- ** Scalar & X
              (.*^),
              (./^),
              (.*#),
              (./#),
              -- ** AccMatrix & Vector
              (#*^),
              (^*#),
              -- ** AccVector & Vector
              (^+^),
              (^-^),
              (^*^),
              -- ** AccMatrix & Matrix
              (#+#),
              (#-#),
              (#*#),
              (#**.),
              -- ** Utility functions
              transpose,
              )
              where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as I

import           Data.Proxy (Proxy(..))
import           GHC.TypeLits ( Nat, KnownNat, natVal)
import           Data.Array.Accelerate ( (:.)(..), Array
                                       , Exp
                                       , DIM0, DIM1, DIM2, DIM3, Z(Z)
                                       , IsFloating, IsNum, Elt, Acc
                                       , All(All), Any(Any)
                                       )


newtype AccScalar a = AccScalar { unScalar :: Acc (Array DIM0 a)}
                    deriving (Show)

-- | A typesafe way to represent an AccVector and its dimension
newtype AccVector (dim :: Nat) a = AccVector { unVector :: Acc (Array DIM1 a)}
                                 deriving (Show)

instance forall n a. (KnownNat n, Eq a, Elt a) => Eq (AccVector n a) where
    v == w = let v' = I.run $ unVector v
                 w' = I.run $ unVector w
              in A.toList v' == A.toList w'


-- | A typesafe way to represent an AccMatrix and its rows/colums
newtype AccMatrix (rows :: Nat) (cols :: Nat) a = AccMatrix {unMatrix :: Acc (Array DIM2 a)}
                                                deriving (Show)

instance forall m n a. (KnownNat m, KnownNat n, Eq a, Elt a) => Eq (AccMatrix m n a) where
    v == w = let v' = I.run $ unMatrix v
                 w' = I.run $ unMatrix w
              in A.toList v' == A.toList w'

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

(#*^) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => AccMatrix m n a -> AccVector n a -> AccVector n a
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
  where m'  = fromIntegral $ natVal (Proxy :: Proxy m) :: Int

infixl 7 #*^

(^*#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => AccVector m a -> AccMatrix m n a -> AccVector n a
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
  where n'  = fromIntegral $ natVal (Proxy :: Proxy n) :: Int

infixr 7 ^*#

(^+^), (^-^) :: forall n a. (KnownNat n, IsNum a, Elt a)
             => AccVector n a -> AccVector n a -> AccVector n a
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
      => AccVector n a -> AccVector n a -> AccScalar a
-- | the usual vector addition/subtraction
--
-- > ⎛v₁⎞   ⎛w₁⎞
-- > ⎜v₂⎟   ⎜w₂⎟
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎜. ⎟ * ⎜. ⎟ = v₁*w₁ + v₂*w₁ + … + vₙ*wₙ
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎜. ⎟   ⎜. ⎟
-- > ⎝vₙ⎠   ⎝wₙ⎠

v ^*^ w = AccScalar $ A.sum $ A.zipWith (*) (unVector v) (unVector w)

infixl 7 ^*^

(#+#), (#-#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
             => AccMatrix m n a -> AccMatrix m n a -> AccMatrix m n a
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
      => AccMatrix k m a -> AccMatrix m n a -> AccMatrix k n a
-- | the usual vector addition/subtraction
--
-- > ⎛ v₁₁ v₁₂ … v₁ₘ ⎞     ⎛ w₁₁ w₁₂ … w₁ₙ ⎞     ⎛ (v₁₁*w₁₁+v₁₂*w₂₁+…+v₁ₘ*wₘ₁) . . . (v₁₁*w₁ₙ+v₁₂*w₂ₙ+…+v₁ₘ*wₘₙ) ⎞
-- > ⎜ v₂₁ v₂₂ … v₂ₘ ⎟     ⎜ w₂₁ w₂₂ … w₂ₙ ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟  *  ⎜  .   .     .  ⎟  =  ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎜  .   .     .  ⎟     ⎜  .   .     .  ⎟     ⎜            .                                  .               ⎟
-- > ⎝ vₖ₁ vₖ₂ … vₖₘ ⎠     ⎝ wₘ₁ wₘ₂ … wₘₙ ⎠     ⎝ (vₖ₁*w₁₁+vₖ₂*w₂₁+…+vₖₘ*wₘ₁) . . . (vₖ₁*w₁ₙ+vₖ₂*w₂ₙ+…+vₖₘ*wₘₙ) ⎠

v #*# w = AccMatrix $ A.fold1 (+)
                    $ A.backpermute (A.lift $ Z:.ek:.en:.em ) reindex
                    $ A.zipWith (*) v' w'
  where [k',m',n'] = map fromIntegral [ natVal (Proxy :: Proxy k)
                                      , natVal (Proxy :: Proxy m)
                                      , natVal (Proxy :: Proxy n)] :: [Int]
        [ek,em,en] = map fromIntegral [k',m',n'] :: [Exp Int]
        v' = A.replicate (A.lift $ Any:.All:.All:.k') (unMatrix v)
        w' = A.replicate (A.lift $ Any:.n':.All:.All) (unMatrix w)
        reindex :: Exp DIM3 -> Exp DIM3
        reindex ix = let (Z:.i:.t:.j) = A.unlift ix
                      in  A.lift (Z:.i:.j:.t :: Z :. Exp Int :. Exp Int :. Exp Int)

infixl 7 #*#

(.*^) :: forall n a. (KnownNat n, IsNum a, Elt a)
      => Exp a -> AccVector n a -> AccVector n a
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
      => Exp a -> AccVector n a -> AccVector n a
a ./^ v = let v' = unVector v
          in AccVector $ A.map (/ a) v'

infixl 7 .*^
infixl 7 ./^

(.*#) :: forall m n a. (KnownNat m, KnownNat n, IsNum a, Elt a)
      => Exp a -> AccMatrix m n a -> AccMatrix m n a
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
      => Exp a -> AccMatrix m n a -> AccMatrix m n a
a ./# v = let v' = unMatrix v
          in AccMatrix $ A.map (/ a) v'

infixl 7 .*#
infixl 7 ./#

(#**.) :: forall n a. (KnownNat n, IsNum a, Elt a)
       => AccMatrix n n a -> Int -> AccMatrix n n a
-- > ⎛ v₁₁ v₁₂ … v₁ₙ ⎞ k
-- > ⎜ v₂₁ v₂₂ … v₂ₙ ⎟
-- > ⎜  .   .     .  ⎟
-- > ⎜  .   .     .  ⎟ 
-- > ⎜  .   .     .  ⎟
-- > ⎜  .   .     .  ⎟
-- > ⎝ vₙ₁ vₙ₂ … vₙₙ ⎠

_ #**. 0 = identityMatrix
v #**. 1 = v
v #**. i | i < 0 = error $ "no negative exponents allowed in matrix exponetiation,"
                        ++ "inverse function not yet implemented"
         | otherwise = (v#**. (i-1)) #*# v

infixr 8 #**.

identityMatrix :: forall n a. (KnownNat n, IsNum a, Elt a) => AccMatrix n n a
identityMatrix = AccMatrix $ A.use $ A.fromFunction (Z:.n':.n') aux
  where aux :: DIM2 -> a
        aux (Z:.i:.j) = if i == j then 1 else 0
        n' = fromIntegral $ natVal (Proxy :: Proxy n)

transpose :: forall m n a. (KnownNat m, KnownNat n, Elt a)
          => AccMatrix m n a -> AccMatrix n m a
transpose = AccMatrix . A.transpose . unMatrix

