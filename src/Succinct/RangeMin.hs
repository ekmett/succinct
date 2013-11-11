{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.RangeMin
  ( RangeMin(..)
  , fromBits
  ) where

import Succinct.Dictionary.Class
import Succinct.Internal.Bit
import Succinct.Internal.Level
import Data.Bits
import Data.Vector.Internal.Check as Ck
import Data.Vector.Primitive as P
import Data.Vector.Unboxed as U
import Data.Word

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data RangeMin = RangeMin
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(P.Vector Word64)
                 [Level] -- last 2 levels should be used only for findClose, otherwise use broadword techniques on the word64s

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

rank_1 :: RangeMin -> Int -> Int
rank_1 = undefined -- (RangeMin n bs ls) i0 = undefined

fromBits :: U.Vector Bit -> RangeMin
fromBits (V_Bit n bs) = RangeMin n bs (levels bs)
{-# INLINE fromBits #-}
