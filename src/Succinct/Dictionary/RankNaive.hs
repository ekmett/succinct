{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.Dictionary.RankNaive
  ( RankNaive(..)
  , rankNaive
  ) where

import Control.Applicative
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Vector.Internal.Check as Ck
import Data.Word
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class
import Succinct.Internal.Bit

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data RankNaive = RankNaive {-# UNPACK #-} !Int !(P.Vector Word64) !(P.Vector Int)
  deriving (Eq,Ord,Show)

instance Access Bool RankNaive where
  size (RankNaive n _ _) = n
  {-# INLINE size #-}

  (!) (RankNaive n bs _) i
     = BOUNDS_CHECK(checkIndex) "RankNaive.!" i n
     $ testBit (P.unsafeIndex bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Bitwise RankNaive where
  bitwise (RankNaive n v _) = V_Bit n v
  {-# INLINE bitwise #-}

instance Dictionary Bool RankNaive

instance Select0 RankNaive
instance Select1 RankNaive

instance Ranked RankNaive where
  rank1 t@(RankNaive n _ _) i =
    BOUNDS_CHECK(checkIndex) "rank" i (n+1)
    $ unsafeRank1 t i
  {-# INLINE rank1 #-}

  unsafeRank1 (RankNaive _ ws ps) i =
    P.unsafeIndex ps w + popCount (P.unsafeIndex ws w' .&. (unsafeBit (bt i) - 1))
    where w = wd i
          -- If we just used w for indexing in ws, it could result in
          -- an out-of-bounds index (for i == n and n `mod` 64 == 0).
          w' = wd (i - 1) - (i - 1) `unsafeShiftR` 63
  {-# INLINE unsafeRank1 #-}

rankNaive :: Bitwise t => t -> RankNaive
rankNaive t = case bitwise t of
  V_Bit n v -> RankNaive n v $ P.scanl (\a b -> a + popCount b) 0 v
{-# INLINE [0] rankNaive #-}
{-# RULES "rankNaive" rankNaive = id #-}

data BuildNaive a = BuildNaive
  {-# UNPACK #-} !Int  -- current rank
  a                    -- rank builder

naiveBuilder :: Builder Word64 (P.Vector Int)
naiveBuilder = Builder $ case vector of
  Builder (Building kr hr zr) -> Building stop step start
    where
      start = BuildNaive 0 <$> zr
      step (BuildNaive r rs) w
        = BuildNaive (r + popCount w) <$> hr rs r
      stop (BuildNaive r rs)
        = hr rs r >>= kr

instance Buildable Bool RankNaive where
  builder = Builder $ case naiveWordBuilder of
    Builder nwb -> wordToBitBuilding nwb fixSize
    where
      naiveWordBuilder = f <$> vector <*> naiveBuilder
        where
          f ws rs = RankNaive (P.length ws `shiftL` 6) ws rs
      fixSize n (RankNaive _ ws rs) = return $ RankNaive n ws rs
  {-# INLINE builder #-}
