module Succinct.RangeMin.Internal.Delta
  ( Delta(..)
  , bool
  , bits
  , byte
  , e8, m8, n8
  ) where

import Data.Bits
import Data.Int
import Data.Semigroup
import Data.Vector.Unboxed.Base as UB
import Data.Vector.Unboxed as U
import Data.Vector.Primitive as P
import Data.Word

-- | Δ-Range-Min
--
-- This provides a semigroup-based variant of the basic aggregation used
-- in <http://arxiv.org/pdf/1111.5220.pdf Grossi and Ottaviano's> Range-Min tree,
-- which is in turn a simplification of a
-- <https://www.siam.org/proceedings/alenex/2010/alx10_009_arroyuelod.pdf Range Min-Max> tree
data Delta = Delta
  { excess  :: {-# UNPACK #-} !Int
  , minima  :: {-# UNPACK #-} !Int
  , nminima :: {-# UNPACK #-} !Int
  } deriving Show

instance Semigroup Delta where
  Delta e m n <> Delta e' m' n' | m'' <- e + m' = case compare m m'' of
    LT -> Delta (e + e') m   n
    EQ -> Delta (e + e') m   (n + n')
    GT -> Delta (e + e') m'' n'
  {-# INLINE (<>) #-}

bool :: Bool -> Delta
bool True  = Delta 1 1 1
bool False = Delta (-1) (-1) 1
{-# INLINE bool #-}

bits :: Bits a => a -> Delta
bits w = Prelude.foldr1 (<>) $ fmap (bool . testBit w) [0..bitSize w - 1]
{-# SPECIALIZE bits :: Word64 -> Delta #-}
{-# SPECIALIZE bits :: Word16 -> Delta #-}
{-# SPECIALIZE bits :: Word8  -> Delta #-}

e8s, m8s, n8s :: P.Vector Int8
(e8s, m8s, n8s) = case U.fromListN 256 $ fmap go [0..255 :: Word8] of
  V_3 _ (V_Int8 es) (V_Int8 ms) (V_Int8 ns) -> (es, ms, ns)
 where
  go i = case bits i of
    Delta e m n -> (fromIntegral e, fromIntegral m, fromIntegral n)

-- | Look up the 'Delta' for a Word8 via LUTs
byte :: Word8 -> Delta
byte w = Delta (e8 w) (m8 w) (n8 w)
{-# INLINE byte #-}

e8 :: Word8 -> Int
e8 w = fromIntegral $ P.unsafeIndex e8s (fromIntegral w)
{-# INLINE e8 #-}

m8 :: Word8 -> Int
m8 w = fromIntegral $ P.unsafeIndex m8s (fromIntegral w)
{-# INLINE m8 #-}

n8 :: Word8 -> Int
n8 w = fromIntegral $ P.unsafeIndex n8s (fromIntegral w)
{-# INLINE n8 #-}
