module Main (main) where

import Criterion.Main
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Word
import Succinct.Sequence
import System.Random

sampleVec :: Int -> [((Int, String), Int)]
sampleVec elements = zip [(i, "v" ++ show i) | i <- [0..]] $ take elements $ randomRs (1, 10000) (mkStdGen 42)

-- huTucker :: (Show a, Eq a) => [(a, Int)] -> String
-- huTucker = show . buildOptimizedAlphabeticalSearchTree

benchSized :: String -> Int -> Benchmark
benchSized tag bytes = v `seq` bgroup tag [
  bench "Hu-Tucker" $ whnf (show . huTucker) v
  ]
  where
    v = sampleVec bytes

benchmarks :: [Benchmark]
benchmarks = [
  benchSized "2^4" (2^4),
  benchSized "2^8" (2^8),
  benchSized "2^12" (2^12),
  benchSized "2^15" (2^15),
  benchSized "2^16" (2^16),
  benchSized "2^17" (2^17),
  benchSized "2^18" (2^18),
  benchSized "2^20" (2^20)
  ]

main :: IO ()
main = defaultMain benchmarks
