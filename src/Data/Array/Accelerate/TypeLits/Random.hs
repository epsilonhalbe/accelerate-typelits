{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Data.Array.Accelerate.TypeLits.Random
                     ( rndMatrix
                     , rndVector
                     , module X
                     ) where

import Data.Array.Accelerate.TypeLits

import           GHC.TypeLits
import           Data.Proxy
import qualified Data.Array.Accelerate as A
import           Data.Array.Accelerate (Elt, Z(..), (:.)(..))
import           Data.Array.Accelerate.System.Random.MWC
import           System.Random.MWC.Distributions as X


rndMatrix :: forall m n e. (KnownNat m, KnownNat n, Elt e) => (GenIO -> IO e) -> IO (AccMatrix m n e)
-- | mwc random provides a fast and "statistically-safe" random function
rndMatrix cdf = undefined -- do _r <- randomArray (const $ cdf) sh 
                   {-return undefined-}
  where m' = fromInteger $ natVal (Proxy :: Proxy m)
        n' = fromInteger $ natVal (Proxy :: Proxy n)
        sh = Z:.m':.n'

rndVector :: forall n e. (KnownNat n, Elt e) => (GenIO -> IO e) -> IO (AccVector n e)
-- | mwc random provides a fast and "statistically-safe" random function
rndVector cdf = undefined -- do _r <- randomArray (const $ cdf) sh
                   {-return undefined-}
  where n' = fromInteger $ natVal (Proxy :: Proxy n)
        sh = Z:.n'
