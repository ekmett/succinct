module Main (main) where

import Criterion.Main
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Word
import Succinct.Sequence
import System.Random

sampleVec :: Int -> [((Int, String), Int)]
sampleVec elements = zip [(i, "v" ++ show i) | i <- [0..]] $ take elements $ randomRs (1, 10000) (mkStdGen 42)

benchSized :: String -> Int -> Benchmark
benchSized tag bytes = v `seq` bgroup tag [
  bench "Hu-Tucker" $ whnf (show . huTucker) v
  ]
  where
    v = sampleVec bytes

benchmarks :: [Benchmark]
benchmarks = [ benchSized ("2^" ++ show i) (2^i)
             | i <- [4,8,12,16,20::Int]]

main :: IO ()
main = defaultMain benchmarks
