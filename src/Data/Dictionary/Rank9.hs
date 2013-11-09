{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Dictionary.Rank9 where

import Data.Bits
import Data.Dictionary.Class
import Data.Vector.Unboxed as U
import Data.Vector.Internal.Check as Ck
import Data.Word

#define BOUNDS_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Bounds)

data Rank9 = Rank9 {-# UNPACK #-} !Int !(U.Vector Word64) !(U.Vector Int)
  deriving (Eq,Ord,Show)

instance Dictionary Rank9 where
  type Elem Rank9 = Bool
  size (Rank9 n _ _) = n
  {-# INLINE size #-}
  rank True xs n = rank1 xs n
  rank False xs n = n - rank1 xs n
  {-# INLINE rank #-}

rank1 :: Rank9 -> Int -> Int
rank1 (Rank9 n ws ps) i
  = BOUNDS_CHECK(checkIndex) "rank" i n
  $ (ps U.! w) + popCount ((ws U.! w) .&. (bit (bt i + 1) - 1))
  where w = wd i

wd :: Int -> Int
wd x = unsafeShiftR x 6
{-# INLINE wd #-}

bt :: Int -> Int
bt x = x .&. 63
{-# INLINE bt #-}
