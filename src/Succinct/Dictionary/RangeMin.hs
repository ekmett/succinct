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

data RangeMin = RangeMin
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(P.Vector Word64)
                 (V.Vector Level) -- last 2 levels should be used only for findClose, otherwise use broadword techniques on the word64s
  deriving Show

rangeMin :: Bitwise t => t -> RangeMin
rangeMin t = case bitwise t of
  V_Bit n bs -> RangeMin n bs $ V.fromList $ levels bs
{-# RULES "rangeMin" rangeMin = id #-}
{-# INLINE [0] rangeMin #-}

instance Access Bool RangeMin where
  size (RangeMin n _ _) = n
  (!)  (RangeMin n bs _) i
     = BOUNDS_CHECK(checkIndex) "RangeMin.!" i n
     $ testBit (P.unsafeIndex bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Bitwise RangeMin where
  bitwise (RangeMin n bs _) = V_Bit n bs

instance Dictionary Bool RangeMin where
  rank True m i = rank_1 m i
  rank False m i = i - rank_1 m i
  {-# INLINE rank #-}

instance Select1 RangeMin
instance Select0 RangeMin

rank_1 :: RangeMin -> Int -> Int
rank_1 (RangeMin n ws ls) i0
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

