{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Data.Array.Accelerate.TypeLits.System.Random.MWC
                     ( rndMatrixWith
                     , rndVectorWith
                     , module Distributions
                     ) where

import Data.Array.Accelerate.TypeLits.Internal

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Elt, Z(..), (:.)(..))
import           Data.Array.Accelerate.System.Random.MWC
import           System.Random.MWC.Distributions as Distributions


rndMatrixWith :: forall m n e. (KnownNat m, KnownNat n, Elt e) => (GenIO -> IO e) -> IO (AccMatrix m n e)
-- | mwc random provides a fast and "statistically-safe" random distribution to
-- work with this
rndMatrixWith cdf = do r <- randomArray (const cdf) sh
                       return $ AccMatrix $ A.use r
  where m' = fromInteger $ natVal (Proxy :: Proxy m)
        n' = fromInteger $ natVal (Proxy :: Proxy n)
        sh = Z:.m':.n'

rndVectorWith :: forall n e. (KnownNat n, Elt e) => (GenIO -> IO e) -> IO (AccVector n e)
-- | mwc random provides a fast and "statistically-safe" random distribution to
-- work with this
rndVectorWith cdf = do r <- randomArray (const cdf) sh
                       return $ AccVector $ A.use r
  where n' = fromInteger $ natVal (Proxy :: Proxy n)
        sh = Z:.n'
