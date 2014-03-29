{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.Dictionary.RangeMin
  ( RangeMin(..)
  , rangeMin
  ) where

import Succinct.Dictionary.Class
import Succinct.Internal.Bit
import Succinct.Internal.Level
import Data.Bits
import Data.Vector.Internal.Check as Ck
import Data.Vector as V
import Data.Vector.Primitive as P
import Data.Word

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

-- | This provides a variant of the basic aggregation used
-- in <http://arxiv.org/pdf/1111.5220.pdf Grossi and Ottaviano's> Range-Min tree,
-- which is in turn a simplification of a
-- <https://www.siam.org/proceedings/alenex/2010/alx10_009_arroyuelod.pdf Range Min-Max> tree

data RangeMin = RangeMin
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(P.Vector Word64)
                 (V.Vector Level) -- last 2 levels should be used only for findClose, otherwise use broadword techniques on the word64s
  deriving Show

rangeMin :: Bitwise t => t -> RangeMin
rangeMin t = case bitwise t of
  V_Bit n bs -> RangeMin n bs $ V.fromList $ levels bs

instance Access Bool RangeMin where
  size (RangeMin n _ _) = n
  (!)  (RangeMin n bs _) i
     = BOUNDS_CHECK(checkIndex) "RangeMin.!" i n
     $ testBit (P.unsafeIndex bs $ wd i) (bt i)

instance Bitwise RangeMin where
  bitwise (RangeMin n bs _) = V_Bit n bs

instance Dictionary Bool RangeMin where
  rank True xs i = rank1_RangeMin xs i
  rank False xs i = i - rank1_RangeMin xs i

instance Select1 RangeMin where
  select1 = select True

instance Select0 RangeMin where
  select0 = select False

instance Ranked RangeMin where
  rank1 = rank1_RangeMin
  rank0 xs i = i - rank1_RangeMin xs i

rank1_RangeMin :: RangeMin -> Int -> Int
rank1_RangeMin (RangeMin n ws ls) i0
  = BOUNDS_CHECK(checkIndex) "rank" i0 (n+1)
  $ go w (V.length ls - 3) $ popCount $ (ws P.! w) .&. (bit (bt i0) - 1)
  where
    w = wd i0
    go !_ 0  !acc = acc
    go i  li acc  = go (unsafeShiftR i 1) (li-1) $!
      if i .&. 1 == 0 then acc else acc + case V.unsafeIndex ls li of
        L8  _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)
        L16 _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)
        L64 _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)
