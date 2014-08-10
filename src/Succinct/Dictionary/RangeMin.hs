{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Succinct.Dictionary.RangeMin
  ( RangeMin(..)
  , rangeMin
  , forwardSearch, backwardSearch
  , findClose, findOpen
  , enclose, levelAncestor
  ) where

import Succinct.Dictionary.Class
import Succinct.Internal.Bit as B
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
-- <http://www.captura.uchile.cl/bitstream/handle/2250/15669/Arroyuelo_Diego.pdf?sequence=1 Range Min-Max> tree

data RangeMin = RangeMin
  { _rangeMinSize   :: {-# UNPACK #-} !Int
  , _rangeMinRaw    :: {-# UNPACK #-} !(P.Vector Word64)
  , _rangeMinLevels :: (V.Vector Level) -- last 2 levels should be used only for findClose, otherwise use broadword techniques on the word64s
  }  deriving Show

rangeMin :: Bitwise t B.Vector => t -> RangeMin
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

instance Bitwise RangeMin B.Vector where
  bitwise (RangeMin n bs _) = V_Bit n bs
  {-# INLINE bitwise #-}

instance Dictionary Bool RangeMin where
  rank True xs i = rank1 xs i
  rank False xs i = i - rank1 xs i

instance Select1 RangeMin where
  select1 = select True

instance Select0 RangeMin where
  select0 = select False

instance Ranked RangeMin where
  rank1 xs i = unsafeShiftR (excess_RangeMin xs i + i) 1
  rank0 xs i = i - rank1 xs i

  excess = excess_RangeMin

excess_RangeMin :: RangeMin -> Int -> Int
excess_RangeMin (RangeMin n ws ls) i0
  = BOUNDS_CHECK(checkIndex) "rank" i0 (n+1)
  $ go w (V.length ls - 3) $ 2 * ones - b
  where
    w = wd i0
    b = bt i0
    ones = popCount $ (ws P.! w) .&. (bit b - 1)
    go !_ 0  !acc = acc
    go i  li acc  = go (unsafeShiftR i 1) (li-1) $!
      if i .&. 1 == 0 then acc else acc + case V.unsafeIndex ls li of
        L8  _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)
        L16 _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)
        L64 _ es _ _ -> fromIntegral $ P.unsafeIndex es (i-1)

-- |
-- @'forwardSearch' r i d@ finds the minimum @j > i@ such that @'excess' r i j = d@
forwardSearch :: RangeMin -> Int -> Int -> Int
forwardSearch = undefined

-- |
-- @'forwardSearch' r i d@ finds the maximum @j < i@ such that @'excess' r i j = d@
backwardSearch :: RangeMin -> Int -> Int -> Int
backwardSearch = undefined

findClose :: RangeMin -> Int -> Int
findClose r x = forwardSearch r x 0

findOpen :: RangeMin -> Int -> Int
findOpen r x = backwardSearch r x 0

enclose :: RangeMin -> Int -> Int
enclose r x = backwardSearch r x 2

levelAncestor :: RangeMin -> Int -> Int -> Int
levelAncestor r x d = backwardSearch r x (d + 1)
