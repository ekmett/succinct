module Main (main) where

import Criterion.Main
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Succinct.Internal.Bit
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9
import Succinct.Dictionary.RankNaive


rankSum :: Ranked r => r -> P.Vector Int -> Int
rankSum r is = r `seq` P.foldl' (\s i -> s + unsafeRank1 r i) 0 is
{-# INLINE rankSum #-}

sampleBitVec :: Int -> U.Vector Bit
sampleBitVec bytes = V_Bit bits $ P.replicate hwords 0x0102040800402010 P.++ P.replicate hwords 0xfefdfbf7ffbfdfef
  where
    bits = bytes * 8
    hwords = bytes `div` 16

sampleIndices :: Int -> Int -> P.Vector Int
sampleIndices bytes count = P.tail $ P.generate (count + 1) (\i -> i * 1758971237 `mod` bits)
  where
    bits = bytes * 8

benchSized :: String -> Int -> Int -> Benchmark
benchSized tag bytes k = is `seq` naive `seq` r9 `seq` bgroup tag [
  bench "naive" $ whnf (rankSum naive) is,
  bench "rank9" $ whnf (rankSum r9) is
  ]
  where
    bv = sampleBitVec bytes
    r9 = rank9 bv
    naive = rankNaive bv
    is = sampleIndices bytes k

benchmarks :: [Benchmark]
benchmarks = [
  benchSized "32k" 32000 1000,
  benchSized "2M" 2000000 10000,
  benchSized "64M" 64000000 1000000
  ]

main :: IO ()
main = defaultMain benchmarks
