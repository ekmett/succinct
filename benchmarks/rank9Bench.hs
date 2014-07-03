{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Criterion.Main
import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Succinct.Internal.Bit
import Succinct.Dictionary.Class
import Succinct.Dictionary.Poppy
import Succinct.Dictionary.Rank9
import Succinct.Dictionary.RankNaive

rankSum' :: Ranked r => r -> Int -> Int -> Int -> Int
rankSum' !r !mask !add n = go 0 0 n
  where
    go !acc !i !k | k == 0    = acc
                  | otherwise = go (acc + unsafeRank1 r i) ((i + add) .&. mask) (k-1)
{-# INLINE rankSum' #-}

sampleBitVec :: Int -> U.Vector Bit
sampleBitVec bytes = V_Bit bits $ P.replicate hwords 0x0102040800402010 P.++ P.replicate hwords 0xfefdfbf7ffbfdfef
  where
    bits = bytes * 8
    hwords = bytes `div` 16

benchSized :: String -> Int -> Int -> Benchmark
benchSized tag magn k = naive `seq` r9 `seq` poppy' `seq` bgroup tag [
  bench "naive" $ whnf (rankSum' naive mask add) k,
  bench "rank9" $ whnf (rankSum' r9 mask add) k,
  bench "poppy" $ whnf (rankSum' poppy' mask add) k
  ]
  where
    bytes = bit magn
    bv = sampleBitVec bytes
    r9 = rank9 bv
    naive = rankNaive bv
    poppy' = poppy bv
    bitMagn = magn + 3
    mask = bit bitMagn - 1
    add = floor $ fromIntegral (bit bitMagn :: Int) * (sqrt 5 + 1) / (2 :: Double)


benchmarks :: [Benchmark]
benchmarks = [
  benchSized  "32k" 15 10000,
  benchSized   "2M" 21 100000,
  benchSized  "64M" 26 1000000,
  benchSized "256M" 28 10000000
  ]

main :: IO ()
main = defaultMain benchmarks
