{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
-- | Mihai Pǎtraşcu's "Succincter" spill-over representations
module Succinct.Internal.Spill
  ( Spill(..)
  , size
  , redundancy
  , integral
  ) where

import Control.Monad.ST
import Data.Bits
import Data.Foldable as Foldable
import Data.Typeable
import Data.Profunctor
import Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Unboxed as U
import Data.Word
import Succinct.Internal.Broadword
import Succinct.Internal.Bit

data Spill a b = Spill
  { bits     :: Int
  , spill    :: Word64
  , universe :: Integer
  , encode   :: forall s. a -> UM.MVector s Bit -> ST s Word64
  , decode   :: U.Vector Bit -> Word64 -> b
  } deriving (Functor, Typeable)

instance Profunctor Spill where
  dimap f g (Spill b s u e d) = Spill b s u (e . f) $ \m k -> g (d m k)
  {-# INLINE dimap #-}

-- | enlarged universe size
size :: Spill a b -> Integer
size a = shiftL (fromIntegral $ spill a) (bits a)
{-# INLINE size #-}

-- | # of bits wasted by the spill-over encoding due to the enlarged universe
redundancy :: Spill a b -> Double
redundancy s = logBase 2 $ fromIntegral (size s) / fromIntegral (universe s)
{-# INLINE redundancy #-}

-- |
-- Mihai's "Lemma 3"
--
-- @integral n r@ designs a spill-over scheme assuming that
--
-- @0 < r <= n@
--
-- Moreover implementation details assume @r < 2^32@, so the
-- spill @K:@@r <= K <= 2*r@ fits in a @Word64@
--
-- The resulting scheme has bounded redundancy:
--
-- @redundancy n r <= 2/fromIntegral r@
integral :: (FiniteBits a, Integral a) => a -> a -> Spill a a
integral n r = Spill
  { bits = m
  , spill = fromIntegral $ unsafeShiftR (n + bit m - 1) m
  , universe = fromIntegral n
  , encode = \a ms -> do
    Foldable.forM_ [0..m-1] $ \i ->
      UM.unsafeWrite ms i $ Bit (testBit a i)
    return $ fromIntegral $ shiftR a m
  , decode = \ms k -> let
      step i x = case U.unsafeIndex ms i of
        Bit True -> setBit x i
        _        -> x
    in Prelude.foldr step (unsafeShiftL (fromIntegral k) m) [0..m-1]
  } where m = msb $ fromIntegral $ div n r -- this doesn't work for big Integers
