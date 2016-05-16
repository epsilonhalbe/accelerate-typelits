{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}


-- Requires files from http://yann.lecun.com/exdb/mnist/
import Codec.Compression.GZip (decompress)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import System.Environment
import System.Exit
import System.IO
import Data.Ord
import GHC.Int (Int64)

import Control.Monad
import Data.List hiding (transpose)
import Data.Array.Accelerate ( Z(..),(:.)(..), Array
                             , DIM1, DIM2, Shape, Exp
                             , Slice, IsNum, Elt, Acc
                             , Any(Any), All(All))

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.System.Random.MWC
import System.Random.MWC.Distributions
import GHC.TypeLits

import Data.Array.Accelerate.TypeLits as T
import Data.Array.Accelerate.TypeLits.System.Random.MWC as R

-- bias and weight can be combined in a single matrix that acts as an affine
-- transformation, using the following observation about matrix multiplication.
--
-- ⎛b₁ w₁₁ w₁₂  … w₁ₙ ⎞   ⎛1 ⎞    ⎛ b₁*1 + w₁₁*x₁ + w₁₂*x₂ +  … w₁ₙ*xₙ ⎞
-- ⎜b₂ w₂₁ w₂₂  … w₂ₙ ⎟   ⎜x₁⎟    ⎜ b₁*1 + w₂₁*x₁ + w₂₂*x₂ +  … w₂ₙ*xₙ ⎟
-- ⎜            …     ⎟   ⎜x₂⎟    ⎜                 …                  ⎟
-- ⎜            …     ⎟ ✕ ⎜  ⎟ =  ⎜                 …                  ⎟
-- ⎜            …     ⎟   ⎜  ⎟    ⎜                 …                  ⎟
-- ⎜            …     ⎟   ⎜  ⎟    ⎜                 …                  ⎟
-- ⎝bₘ  wₘ₁ wₘ₂ … wₘₙ ⎠   ⎝xₙ⎠    ⎝ bₘ*1 + wₘ₁*x₁ + wₘ₂*x₂ +  … wₘₙ*xₙ ⎠
--
--  bias/weights          input          output/input for next layer

data NeuralNetwork = NN { inpt :: Int
                        , hiddenLayers :: [Int]
                        , outpt :: Int}

-- | for each layer we have a matrix where

--                +---------- input
--                | +-------- hidden layer 1
--                | | +------ hidden layer 2
--                | | | +---- output
--                | | | |
-- neuralNetwork [2,3,4,2] ~ list of matrices with dimensions
--                           rows ✕ cols
--                         | 3✕2 | 4✕3 | 2✕4 |
--
-- ⎡                       |                           |                               ⎤
-- ⎢ ⎛w₁₁ w₁₂⎞ ⎛i₁⎞   ⎛b₁⎞ | ⎛w₁₁ w₁₂ w₁₃⎞ ⎛i₁⎞   ⎛b₁⎞ |                   ⎛i₁⎞        ⎥
-- ⎢ ⎜w₂₁ w₂₂⎟•⎝i₂⎠ + ⎜b₂⎟ | ⎜w₂₁ w₂₂ w₂₃⎟•⎜i₂⎟ + ⎜b₂⎟ | ⎛w₁₁ w₁₂ w₁₃ w₁₄⎞ ⎜i₂⎟   ⎛b₁⎞ ⎥
-- ⎢ ⎝w₃₁ w₃₂⎠        ⎝b₃⎠ | ⎜w₃₁ w₃₂ w₃₃⎟ ⎝i₃⎠   ⎜b₃⎟ | ⎝w₂₁ w₂₂ w₂₃ w₂₄⎠•⎜i₃⎟ + ⎝b₂⎠ ⎥
-- ⎢                       | ⎝w₄₁ w₄₂ w₄₃⎠        ⎝b₄⎠ |                   ⎝i₄⎠        ⎥
-- ⎣                       |                           |                               ⎦

nextLayer :: (KnownNat n, KnownNat m, IsNum a, Elt a)
          => (AccMatrix m n a, AccVector n a) -> AccVector n a -> AccVector n a
nextLayer (weight, bias) input  = weight #*^ input ^+^ bias

ramp :: (Ord a, Num a) => a -> a
-- | the ramp function is our activation function for the neural network
-- ramp function and its derivative is the heaviside function
ramp = max 0

heaviside :: (Ord a, Num a) => a -> a
heaviside x | x < 0      = 0
            | otherwise  = 1

output :: (Foldable t, KnownNat m, KnownNat n, IsNum b, Elt b)
       => t (AccMatrix m n b, AccVector n b) -> AccVector n b -> AccVector n b
output weightBiases input = foldl' aux input weightBiases
  where aux i wb = afmap ramp $ nextLayer wb i


{-# ANN hidden "HLint: ignore Eta reduce" #-}
hidden :: (KnownNat m, KnownNat n, IsNum a, Elt a)
       => AccVector n a -> [(AccMatrix m n a, AccVector n a)] -> ([AccVector n a], [AccVector n a])
-- | calculates the hidden activation vectors and value vectors for each hidden
-- layer
hidden input layers= foldl' aux ([input], []) layers
  where -- aux ::
        aux (activated@(a:_), hidden') (ws, bs) =
            let next = nextLayer (ws, bs) a
             in (afmap ramp next:activated-- calculating the activated next layer
               ,            next:hidden') -- calculating the           next layer
        aux ([],_) _ = error "hiddenLayer-auxiliary function called with ([],_)"

dCost :: (Num a, Ord a) => a -> a -> a
dCost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

deltas :: (KnownNat m, KnownNat n, IsNum a, Elt a)
       => AccVector n a -> AccVector n a -> [(AccMatrix m n a, AccVector n a)] -> ([AccVector n a], [[AccScalar a2]])
-- | calculates the errors between expected output and the result produced by
-- the current state of the neural network
deltas xv yv layers = let (activated@(a:_), h:idden) = hidden xv layers
                          delta0 = zipWithV dCost a yv ^*^ afmap heaviside h
                      in (reverse activated, f ((T.transpose . snd) `afmap` reverse layers) idden [delta0])

   where -- f :: _ -- [WeightMatrix] -> [[Double]] -> [[Double]] -> [[Double]]
         f = undefined
         {-f _ [] dvs = dvs-}
         {-f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $-}
             {-zipWith (*) [row ^*^  dv | row <- wm] (heaviside <$> zv)-}
         {-f _ _ _ = error "f"-}

eta :: Exp Double
eta = 0.002

descend :: (KnownNat n) => AccVector n Double -> AccVector n Double -> AccVector n Double
descend av dv = av ^-^ (eta .*^ dv)
--
-- {-learn :: [Double] -> [Double] -> [([Double], [[Double]])] -> [([Double], [[Double]])]-}
-- learn xv yv layers = let (avs, dvs) = deltas xv yv layers
--   in zip (zipWith descend (fst <$> layers) dvs) $
--     zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
--       (snd <$> layers) avs dvs
--

-- type Picture = AccVector 784 Double

getImage :: Num b => ByteString -> Int64 -> [b]
getImage s n = fromIntegral . BS.index s . (n*28*28 + 16 +) <$> [0..28*28 - 1]
    where height = 28 :: Int
          width  = 28 :: Int
          offset = 16 :: Int

getX :: Fractional b => ByteString -> Int64 -> [b]
getX s n = (/ 256) <$> getImage s n

getLabel :: Num b => ByteString -> Int64 -> b
getLabel s n = fromIntegral $ BS.index s (n + 8)

getY :: Num b => ByteString -> Int64 -> [b]
getY s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9 :: Int]

render :: Int -> Char
render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)


main :: IO ()
main = undefined

-- main :: IO ()
-- main = do
--   as <- getArgs
--   [trainI, trainL, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
--     [ "train-images-idx3-ubyte.gz"
--     , "train-labels-idx1-ubyte.gz"
--     ,  "t10k-images-idx3-ubyte.gz"
--     ,  "t10k-labels-idx1-ubyte.gz"
--     ]
--
--   hSetBuffering stderr LineBuffering
--   let
--     (pStr, pStrLn) = case as of
--       ["print"] -> (hPutStr stderr, hPutStrLn stderr)
--       _         -> (putStr, putStrLn)
--
--   n <- (`mod` 10000) <$> randomIO
--   pStr . unlines $
--     take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)
--
--   b <- newBrain [784, 30, 10]
--   let
--     example = getX testI n
--     bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b [
--      [   0.. 999],
--      [1000..2999],
--      [3000..5999],
--      [6000..9999]]
--     smart = last bs
--     cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
--     bestOf = fst . maximumBy (comparing snd) . zip [0..]
--
--   forM_ bs $ pStrLn . unlines . zipWith cute [0..9 :: Int] . feed example
--
--   pStrLn $ "best guess: " ++ show (bestOf $ feed example smart)
--
--   let guesses :: [Int] = bestOf . (\i -> feed (getX testI i) smart) <$> [0..9999]
--   let answers :: [Int] = getLabel testL <$> [0..9999]
--   putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++ " / 10000"
--
--   case as of
--     ["print"] -> print smart
--     _         -> return ()
