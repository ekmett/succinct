{-# LANGUAGE CPP #-}
module Succinct.Internal.Delta
  ( Delta(..)
  , minima
  , bool
  , bits
  , byte
  , e8, d8, n8
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
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
-- in <http://arxiv.org/pdf/1111.5220.pdf Grossi and Ottaviano's Range-Min tree>,
-- which is in turn a simplification of a
-- <https://www.siam.org/proceedings/alenex/2010/alx10_009_arroyuelod.pdf Range Min-Max> tree
data Delta = Delta
  { excess  :: {-# UNPACK #-} !Int -- even when # bits is even, odd when it is odd
  , delta   :: {-# UNPACK #-} !Int -- minima = excess - delta
  , nminima :: {-# UNPACK #-} !Int -- # of minima
  } deriving Show

instance Semigroup Delta where
  Delta e d n <> Delta e' d' n' = case compare d' (d + e) of
    LT -> Delta (e + e') d'      n'
    EQ -> Delta (e + e') (d + e) (n + n')
    GT -> Delta (e + e') (d + e) n
  {-# INLINE (<>) #-}

minima :: Delta -> Int
minima (Delta e d _) = e - d
{-# INLINE minima #-}

bool :: Bool -> Delta
bool True  = Delta 1 1 1
bool False = Delta (-1) 0 1
{-# INLINE bool #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
bits :: FiniteBits a => a -> Delta
bits w = Prelude.foldr1 (<>) $ fmap (bool . testBit w) [0..finiteBitSize w - 1]
#else
bits :: Bits a => a -> Delta
bits w = Prelude.foldr1 (<>) $ fmap (bool . testBit w) [0..bitSize w - 1]
#endif
{-# SPECIALIZE bits :: Word64 -> Delta #-}
{-# SPECIALIZE bits :: Word16 -> Delta #-}
{-# SPECIALIZE bits :: Word8  -> Delta #-}

e8s, d8s, n8s :: P.Vector Int8
(e8s, d8s, n8s) = case U.fromListN 256 $ fmap go [0..255 :: Word8] of
  V_3 _ (V_Int8 es) (V_Int8 ds) (V_Int8 ns) -> (es, ds, ns)
 where
  go i = case bits i of
    Delta e m n -> (fromIntegral e, fromIntegral m, fromIntegral n)

-- | Look up the 'Delta' for a Word8 via LUTs
byte :: Word8 -> Delta
byte w = Delta (e8 w) (d8 w) (n8 w)
{-# INLINE byte #-}

e8 :: Word8 -> Int
e8 w = fromIntegral $ P.unsafeIndex e8s (fromIntegral w)
{-# INLINE e8 #-}

d8 :: Word8 -> Int
d8 w = fromIntegral $ P.unsafeIndex d8s (fromIntegral w)
{-# INLINE d8 #-}

n8 :: Word8 -> Int
n8 w = fromIntegral $ P.unsafeIndex n8s (fromIntegral w)
{-# INLINE n8 #-}

{-
fwd_e8s :: P.Vector Word8
fwd_e8s = P.fromListN 2048 $ go <$> [0..7] <*> [0..255] where
  go d p = either id id $ Prelude.foldl step (Right 8) [0..7] where
    step (Left i) _ = Left i
    step (Right r) i
      | r' == d   = Left i
      | otherwise = Right r'
      where r' = fromIntegral $ r + fromEnum (testBit p (fromIntegral i))

fwd_e8 :: Word8 -> Int -> Int
fwd_e8 w8 i = fromIntegral $ P.unsafeIndex fwd_e8s (fromIntegral w8 + unsafeShiftL i 8)
-}
