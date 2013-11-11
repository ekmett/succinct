{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , rank9
  ) where

import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Vector.Internal.Check as Ck
import Data.Word
import Succinct.Dictionary.Class
import Succinct.Internal.Bit

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Rank9 = Rank9 {-# UNPACK #-} !Int !(P.Vector Word64) !(P.Vector Int)
  deriving (Eq,Ord,Show)

instance Access Bool Rank9 where
  size (Rank9 n _ _) = n
  {-# INLINE size #-}

  (!) (Rank9 n bs _) i
     = BOUNDS_CHECK(checkIndex) "Rank9.!" i n
     $ testBit (P.unsafeIndex bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Bitwise Rank9 where
  bitwise (Rank9 n v _) = V_Bit n v
  {-# INLINE bitwise #-}

instance Dictionary Bool Rank9 where
  rank True xs n = rank_1 xs n
  rank False xs n = n - rank_1 xs n
  {-# INLINE rank #-}

instance Select0 Rank9
instance Select1 Rank9

rank_1 :: Rank9 -> Int -> Int
rank_1 (Rank9 n ws ps) i
  = BOUNDS_CHECK(checkIndex) "rank" i (n+1)
  $ (ps P.! w) + popCount ((ws P.! w) .&. (bit (bt i) - 1))
  where w = wd i
{-# INLINE rank_1 #-}

rank9 :: Bitwise t => t -> Rank9
rank9 t = case bitwise t of
  V_Bit n v -> Rank9 n v $ P.scanl (\a b -> a + popCount b) 0 v
{-# INLINE [0] rank9 #-}
{-# RULES "rank9" rank9 = id #-}
