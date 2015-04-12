{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Control.Monad
import Data.Bits
import Data.Word (Word8, Word64)
import Data.List (unfoldr)
import Succinct.Internal.Broadword
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit hiding (Test, assert)

case_select_shifted_ones :: IO ()
case_select_shifted_ones =
  forM_ [0..63] $ \i ->
    assertEqual ("for index " ++ show i) i
      (selectWord64 (shiftL 1 i) 0)

listBytes :: Word64 -> [Word8]
listBytes = take 8 . unfoldr (\x -> Just (fromIntegral x, shiftR x 8))

unlistBytes :: [Word8] -> Word64
unlistBytes = foldr1 (\x w -> x .|. shiftL w 8) . map fromIntegral

referenceByteCounts :: Word64 -> Word64
referenceByteCounts = unlistBytes . map (fromIntegral . popCount) . listBytes

prop_byteCounts w =
  referenceByteCounts w == byteCounts w

main :: IO ()
main = $defaultMainGenerator
