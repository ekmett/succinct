module Main (main) where

import Criterion.Main
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Word
import Succinct.Internal.PopCount


sampleVec :: Int -> P.Vector Word64
sampleVec bytes = P.generate (bytes `div` 8) ((1758971237*) . fromIntegral)

popCountNaive :: P.Vector Word64 -> Int
popCountNaive v = P.foldl' (\s x -> s + popCount x) 0 v

popCountByWord :: P.Vector Word64 -> Int
popCountByWord v = P.foldl' (\s x -> s + popCountWord64 x) 0 v

popCountVec :: VectorInternal -> Int
popCountVec vi@(VectorInternal _ len _) = popCountSlice vi 0 len

benchSized :: String -> Int -> Benchmark
benchSized tag bytes = v `seq` vi `seq` bgroup tag [
  bench "naive" $ whnf popCountNaive v,
  bench "by-word" $ whnf popCountByWord v,
  bench "full" $ whnf popCountVec vi
  ]
  where
    v = sampleVec bytes
    vi = vectorToInternal v

benchmarks :: [Benchmark]
benchmarks = [
  benchSized "32k" 32000,
  benchSized "2M" 2000000,
  benchSized "64M" 64000000
  ]

main :: IO ()
main = defaultMain benchmarks
