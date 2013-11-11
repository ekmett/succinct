{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , ToRank9(..)
  ) where

import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Data.Vector.Internal.Check as Ck
import Data.Word
import qualified Succinct.Dictionary.Class as Succinct
import Succinct.Internal.Bit

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Rank9 = Rank9 {-# UNPACK #-} !Int !(P.Vector Word64) !(P.Vector Int)
  deriving (Eq,Ord,Show)

instance Succinct.Access Bool Rank9 where
  size (Rank9 n _ _) = n
  {-# INLINE size #-}

  (!) (Rank9 n bs _) i
     = BOUNDS_CHECK(checkIndex) "Rank9.!" i n
     $ testBit (P.unsafeIndex bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Succinct.Bitwise Rank9 where
  bitwise (Rank9 n v _) = V_Bit n v

instance Succinct.Dictionary Bool Rank9 where
  rank True xs n = rank1 xs n
  rank False xs n = n - rank1 xs n
  {-# INLINE rank #-}

instance Succinct.Select0 Rank9
instance Succinct.Select1 Rank9

rank1 :: Rank9 -> Int -> Int
rank1 (Rank9 n ws ps) i
  = BOUNDS_CHECK(checkIndex) "rank" i (n+1)
  $ (ps P.! w) + popCount ((ws P.! w) .&. (bit (bt i) - 1))
  where w = wd i
{-# INLINE rank1 #-}

class ToRank9 t where
  rank9 :: t -> Rank9
  default rank9 :: Succinct.Bitwise t => t -> Rank9
  rank9 = rank9 . Succinct.bitwise

instance ToRank9 Rank9 where
  rank9 = id

instance b ~ Bit => ToRank9 (U.Vector b) where
  rank9 (V_Bit n v) = Rank9 n v $ P.scanl (\a b -> a + popCount b) 0 v

instance b ~ Bool => ToRank9 [b]
