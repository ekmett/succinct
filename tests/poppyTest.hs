{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Monad
import Succinct.Dictionary.Class
import Succinct.Dictionary.Poppy
import Succinct.Dictionary.RankNaive
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

testForLength :: Int -> IO ()
testForLength n = do
  let bits = take n $ concat $ repeat [True, False, False]
      pp = poppy bits
      naive = rankNaive bits
  forM_ [0..n] $ \i -> do
    assertEqual ("for index " ++ show i) (rank1 naive i) (rank1 pp i)

case_Poppy_is_RankNaive_0 = testForLength 0
case_Poppy_is_RankNaive_1 = testForLength 1
case_Poppy_is_RankNaive_7 = testForLength 7
case_Poppy_is_RankNaive_8 = testForLength 8
case_Poppy_is_RankNaive_63 = testForLength 63
case_Poppy_is_RankNaive_64 = testForLength 64
case_Poppy_is_RankNaive_66 = testForLength 66
case_Poppy_is_RankNaive_511 = testForLength 511
case_Poppy_is_RankNaive_512 = testForLength 512
case_Poppy_is_RankNaive_514 = testForLength 514
case_Poppy_is_RankNaive_2047 = testForLength 2047
case_Poppy_is_RankNaive_2048 = testForLength 2048
case_Poppy_is_RankNaive_2050 = testForLength 2050

testAllOnes :: Int -> IO ()
testAllOnes n = do
  let pp = poppy $ replicate n True
  forM_ [0..n] $ \i -> do
    assertEqual ("for index " ++ show i) i (rank1 pp i)

case_Poppy_all_ones_1 = testAllOnes 1
case_Poppy_all_ones_7 = testAllOnes 7
case_Poppy_all_ones_8 = testAllOnes 8
case_Poppy_all_ones_63 = testAllOnes 63
case_Poppy_all_ones_64 = testAllOnes 64
case_Poppy_all_ones_66 = testAllOnes 66
case_Poppy_all_ones_511 = testAllOnes 511
case_Poppy_all_ones_512 = testAllOnes 512
case_Poppy_all_ones_514 = testAllOnes 514
case_Poppy_all_ones_2047 = testAllOnes 2047
case_Poppy_all_ones_2048 = testAllOnes 2048
case_Poppy_all_ones_2050 = testAllOnes 2050


main :: IO ()
main = $defaultMainGenerator
