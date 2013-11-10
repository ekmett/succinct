{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Trustworthy #-}
module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , fromList
  ) where

import Control.Applicative
import Data.Bits
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Data.Vector.Internal.Check as Ck
import Data.Word
import qualified Succinct.Dictionary.Class as Succinct
import Succinct.Dictionary.Internal.Bit

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Rank9 = Rank9 {-# UNPACK #-} !Int !(P.Vector Word64) !(P.Vector Int)
  deriving (Eq,Ord,Show)

instance Succinct.Access Bool Rank9 where
  size (Rank9 n _ _) = n
  {-# INLINE size #-}

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

fromList :: [Bool] -> Rank9
fromList xs = case U.fromList (Bit <$> xs) of
  V_Bit n v -> Rank9 n v $ P.scanl (\a b -> a + popCount b) 0 v
{-# INLINE fromList #-}
