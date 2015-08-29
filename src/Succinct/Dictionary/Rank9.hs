{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , rank9
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S
import Data.Vector.Internal.Check as Ck
import Data.Word
import Data.Proxy
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class
import Succinct.Internal.Bit as B
import Succinct.Internal.StorableBitVector as SBV
import Succinct.Internal.PopCount

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Rank9 v = Rank9 {-# UNPACK #-} !Int !(v Word64) !(v Word64)

deriving instance Eq (v Word64) => Eq (Rank9 v)
deriving instance Ord (v Word64) => Ord (Rank9 v)
deriving instance Show (v Word64) => Show (Rank9 v)

instance G.Vector v Word64 => Access Bool (Rank9 v) where
  size (Rank9 n _ _) = n
  {-# INLINE size #-}

  (!) (Rank9 n bs _) i
     = BOUNDS_CHECK(checkIndex) "Rank9.!" i n
     $ testBit (G.unsafeIndex bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Bitwise (Rank9 P.Vector) B.Vector where
  bitwise (Rank9 n v _) = V_Bit n v
  {-# INLINE bitwise #-}

instance Bitwise (Rank9 S.Vector) SBV.Vector where
  bitwise (Rank9 n v _) = SBV.SV_Bit n v
  {-# INLINE bitwise #-}

instance (G.Vector v Word64, G.Vector v Bool) => Dictionary Bool (Rank9 v)

instance (G.Vector v Word64, G.Vector v Bool) => Select0 (Rank9 v)
instance (G.Vector v Word64, G.Vector v Bool) => Select1 (Rank9 v)

instance (G.Vector v Word64, G.Vector v Bool) => Ranked (Rank9 v) where
  rank1 t@(Rank9 n _ _) i =
    BOUNDS_CHECK(checkIndex) "rank" i (n+1) $
    unsafeRank1 t i
  {-# INLINE rank1 #-}

  unsafeRank1 (Rank9 _ ws ps) i = result
    where
      wi = wd i
      block = wi `shiftR` 3 `shiftL` 1
      base = G.unsafeIndex ps block
      t = wi .&. 7 - 1
      s = G.unsafeIndex ps (block + 1)
      sShift = (t + t `shiftR` 60 .&. 8) * 9
      count9 = s `unsafeShiftR` sShift .&. 0x1FF
      -- If we just used 'wi' here, we would index out of 'ws' when
      -- i == n and n `mod` 64 == 0. But, whenever i `mod` 64 == 0 we
      -- .&. with 0, so the value read from the ws is effectively
      -- ignored.
      --
      -- The following is a branchless work-around for this: we look
      -- at the previous word whenever i `mod` 64 == 0, except when i
      -- == 0.
      --
      -- TODO(klao): Is this needed? How to handle this better?
      -- Abstract it out into Internal!
      wi' = wd (i - 1) - (i - 1) `unsafeShiftR` 63
      rest = popCountWord64 $ (G.unsafeIndex ws wi') .&. (unsafeBit (bt i) - 1)
      result = fromIntegral (base + count9) + rest
  {-# INLINE unsafeRank1 #-}

rank9 :: forall v t.
         (G.Vector (Packed v) Word64, G.Vector v Word64, Bitwise t v, PackedBits v)
      => Proxy v -> t -> Rank9 (Packed v)
rank9 _ t = case bitwise t :: v Bit of
  v -> Rank9 n (packedBits v) ps
    where
      -- Because we are building word-by-word and not bit-by-bit, we
      -- sometimes build a bigger structure than is strictly
      -- necessary. (In the expression below (n+63) should have been
      -- simply n.)
      k = ((n + 63) `shiftR` 9 + 1) `shiftL` 1
      n = G.length v
      ps = buildWithFoldlM foldlMPadded (r9Builder $ vectorSized k) v
{-# INLINE [0] rank9 #-}
-- {-# RULES "rank9" rank9 = id #-}

data Build9 a = Build9
  {-# UNPACK #-} !Int    -- word count `mod` 8
  {-# UNPACK #-} !Word64 -- current rank
  {-# UNPACK #-} !Word64 -- rank within the current block
  {-# UNPACK #-} !Word64 -- current "rank9" word
  !a                     -- rank vector builder

r9Builder :: G.Vector v Word64
          => Builder Word64 (v Word64) -> Builder Word64 (v Word64)
r9Builder vectorBuilder = Builder $ case vectorBuilder of
  Builder (Building kr hr zr) -> Building stop step start
    where
      start = Build9 0 0 0 0 <$> zr
      step (Build9 n tr br r9 rs) w
        | n == 7 = Build9 0 tr' 0 0 <$> stepRank rs tr r9
        | otherwise = return $ Build9 (n + 1) tr br' r9' rs
        where
          tr' = tr + br'
          br' = br + fromIntegral (popCountWord64 w)
          r9' = r9 .|. br' `unsafeShiftL` (9 * n)
      stepRank rs tr r9 = hr rs tr >>= (`hr` r9)
      stop (Build9 _n tr _br r9 rs)
        = stepRank rs tr r9 >>= kr
{-# INLINE r9Builder #-}

rank9WordBuilder :: G.Vector v Word64 => Builder Word64 (Rank9 v)
rank9WordBuilder = f <$> vector <*> r9Builder vector
  where
    f ws rs = Rank9 (G.length ws `shiftL` 6) ws rs
    {-# INLINE f #-}
{-# INLINE rank9WordBuilder #-}

instance G.Vector v Word64 => Buildable Bool (Rank9 v) where
  builder = Builder $ case rank9WordBuilder of
    Builder r9wb -> wordToBitBuilding r9wb fixSize
      where
        fixSize n (Rank9 _ ws rs) = return $ Rank9 n ws rs
  {-# INLINE builder #-}
