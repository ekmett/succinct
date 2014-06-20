{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Main (main) where

import Control.Monad
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9
import Succinct.Dictionary.RankNaive
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

testForLength :: Int -> IO ()
testForLength n = do
  let bits = take n $ concat $ repeat [True, False, False]
      r9 = rank9 bits
      naive = rankNaive bits
  forM_ [0..n] $ \i -> do
    assertEqual ("for index " ++ show i) (rank1 naive i) (rank1 r9 i)

case_Rank9_is_RankNaive_0 = testForLength 0
case_Rank9_is_RankNaive_1 = testForLength 1
case_Rank9_is_RankNaive_7 = testForLength 7
case_Rank9_is_RankNaive_8 = testForLength 8
case_Rank9_is_RankNaive_63 = testForLength 63
case_Rank9_is_RankNaive_64 = testForLength 64
case_Rank9_is_RankNaive_66 = testForLength 66
case_Rank9_is_RankNaive_511 = testForLength 511
case_Rank9_is_RankNaive_512 = testForLength 512
case_Rank9_is_RankNaive_514 = testForLength 514

testAllOnes :: Int -> IO ()
testAllOnes n = do
  let r9 = rank9 $ replicate n True
  forM_ [0..n] $ \i -> do
    assertEqual ("for index " ++ show i) i (rank1 r9 i)

case_Rank9_all_ones_1 = testAllOnes 1
case_Rank9_all_ones_7 = testAllOnes 7
case_Rank9_all_ones_8 = testAllOnes 8
case_Rank9_all_ones_63 = testAllOnes 63
case_Rank9_all_ones_64 = testAllOnes 64
case_Rank9_all_ones_66 = testAllOnes 66
case_Rank9_all_ones_511 = testAllOnes 511
case_Rank9_all_ones_512 = testAllOnes 512
case_Rank9_all_ones_514 = testAllOnes 514


main :: IO ()
main = $defaultMainGenerator
