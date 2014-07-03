{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Monad
import Data.Bits
import qualified Data.Vector.Primitive as P
import Succinct.Internal.PopCount
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

case_vector_slice :: IO ()
case_vector_slice = do
  let v = P.fromList [1,3,7,15,31,63]
  12 @=? popCountSlice (vectorToInternal v) 2 3

case_all_one_bits :: IO ()
case_all_one_bits = do
  let v = P.replicate 16 (-1)
  forM_ [0..1024] $ \i -> do
    assertEqual ("for index " ++ show i) i
      (popCountBitSlice (vectorToInternal v) 0 i)

prop_pop_count_word64 w =
  popCount w == popCountWord64 w

main :: IO ()
main = $defaultMainGenerator
