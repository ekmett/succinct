-- | Utility functions for the RRR class and offset representation
module Succinct.Internal.Binomial
  ( binomial
  , logBinomial
  , bitmap
  , offset
  ) where

import Control.Monad.ST
import Data.Bits
import Data.Foldable as F
import Data.Function
import Data.List as List (sortBy, scanl)
import Data.Vector as V
import Data.Vector.Primitive as P
import Data.Vector.Primitive.Mutable as PM
import Data.Word

-- maximum binomial term
_N :: Int
_N = 16

bits :: Word16 -> Word8
bits = go 0 where
  go i 0 = i
  go i n = go (i + 1) (unsafeShiftR n 1)
{-# INLINE bits #-}

-- build a fixed sized table of binomial coeffients
-- bins :: (V.Vector (P.Vector Word16), V.Vector (P.Vector Word8))

bin :: V.Vector (P.Vector Word16)
lbin :: V.Vector (P.Vector Word8)
(bin, lbin) = runST $ do
  mbin  <- V.replicateM (_N+2) $ PM.replicate (_N+2) 0
  mlbin <- V.replicateM (_N+2) $ PM.replicate (_N+2) 0
  F.forM_ [0.._N] $ \i -> do
    mbini <- V.unsafeIndexM mbin i
    PM.unsafeWrite mbini 0 1
    PM.unsafeWrite mbini 1 1
    PM.unsafeWrite mbini i 1
    mlbini <- V.unsafeIndexM mlbin i
    PM.unsafeWrite mlbini 0 0
    PM.unsafeWrite mlbini 1 0
    PM.unsafeWrite mlbini i 0
  F.forM_ [1.._N+1] $ \j ->
    F.forM_ [j+1.._N+1] $ \i -> do
      mbinim1 <- V.unsafeIndexM mbin (i - 1)
      mbini   <- V.unsafeIndexM mbin i
      bx <- PM.unsafeRead mbinim1 (j-1)
      by <- PM.unsafeRead mbinim1 j
      let bz = bx + by
      PM.unsafeWrite mbini j bz
      mlbini <- V.unsafeIndexM mlbin i
      PM.unsafeWrite mlbini j (bits bz - 1)
  xs <- V.forM mbin P.unsafeFreeze
  ys <- V.forM mlbin P.unsafeFreeze
  return (xs,ys)

bitmaps :: P.Vector Word16
bitmaps = P.fromListN 65536 $ sortBy (compare `on` popCount) [minBound .. maxBound :: Word16]

classOffsets :: P.Vector Word16
classOffsets = P.fromListN (_N+1) $ List.scanl (\a k -> a + fromIntegral (binomial _N k)) 0 [0.._N]

offsets :: P.Vector Word16
offsets
  = P.fromListN (P.length bitmaps)
  $ fmap (\(w,o) -> o - (classOffsets P.! popCount w))
  $ sortBy (on compare fst)
  $ Prelude.zip (P.toList bitmaps) [0..]

-- @'binomial' n k@ returns @n `choose` k@ for @n, k <= 15@
binomial :: Int -> Int -> Int
binomial n k
  | 0 <= n, n <= _N, 0 <= k, k <= _N = fromIntegral $ P.unsafeIndex (V.unsafeIndex bin n) k
  | otherwise = error "binomial: out of range"
{-# INLINE binomial #-}

logBinomial :: Int -> Int -> Int
logBinomial n k
  | 0 <= n, n <= _N, 0 <= k, k <= _N = fromIntegral $ P.unsafeIndex (V.unsafeIndex lbin n) k
  | otherwise = error "logBinomial: out of range"
{-# INLINE logBinomial #-}

-- | bitmap by class and offset
--
-- There are 17 classes @k@ (based on popCount) each with @binomial 16 k@ possible offsets.
bitmap :: Int -> Int -> Word16
bitmap k o = bitmaps P.! (fromIntegral (classOffsets P.! k) + o)
{-# INLINE bitmap #-}

-- | Calculate the offset of a Word16 into its class.
--
-- You can use @popCount@ to calculate the class.
--
-- @
-- bitmap (popCount w) (offset w) = w
-- @
offset :: Word16 -> Int
offset w = fromIntegral (offsets P.! fromIntegral w)
{-# INLINE offset #-}
