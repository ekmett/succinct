{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , rank9
  ) where

import Control.Applicative
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Vector.Internal.Check as Ck
import Data.Word
import Succinct.Dictionary.Class
import Succinct.Internal.Bit
import Succinct.Internal.Builder

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

instance Dictionary Bool Rank9

instance Select0 Rank9
instance Select1 Rank9

instance Ranked Rank9 where
  rank1 (Rank9 n ws ps) i
    = BOUNDS_CHECK(checkIndex) "rank" i (n+1)
    $ (ps P.! w) + popCount ((ws P.! w) .&. (bit (bt i) - 1))
    where w = wd i
  {-# INLINE rank1 #-}
  rank0 t i = i - rank1 t i
  {-# INLINE rank0 #-}

rank9 :: Bitwise t => t -> Rank9
rank9 t = case bitwise t of
  V_Bit n v -> Rank9 n v $ P.scanl (\a b -> a + popCount b) 0 v
{-# INLINE [0] rank9 #-}
{-# RULES "rank9" rank9 = id #-}

data instance Builder Rank9 s
  = Build9 {-# UNPACK #-} !Int    -- bit position
           {-# UNPACK #-} !Word64 -- current word
           {-# UNPACK #-} !Int    -- current rank
           !(Builder (P.Vector Word64) s) -- words
           !(Builder (P.Vector Int) s)    -- ranks

instance Buildable Rank9 Bool where
  new = Build9 0 0 0 <$> new <*> new
  {-# INLINE new #-}
  snoc (Build9 n w r ws rs) b
    | n63 == 63 = Build9 (n + 1) 0 (r + popCount w') <$> snoc ws w' <*> snoc rs r
    | otherwise = return $ Build9 (n + 1) w' r ws rs
    where w' = if b then setBit w n63 else w
          n63 = n .&. 63
  {-# INLINE snoc #-}
  freeze (Build9 n w r ws rs)
    | n .&. 63 == 0 = Rank9 n <$> freeze ws <*> (snoc rs r >>= freeze)
    | otherwise = Rank9 n <$> (snoc ws w >>= freeze)
                          <*> (snoc rs r >>= \rs' -> snoc rs' (r + popCount w) >>= freeze)
  {-# INLINE freeze #-}
  unsafeFreeze (Build9 n w r ws rs)
    | n .&. 63 == 0 = Rank9 n <$> unsafeFreeze ws <*> (snoc rs r >>= unsafeFreeze)
    | otherwise = Rank9 n <$> (snoc ws w >>= unsafeFreeze)
                          <*> (snoc rs r >>= \rs' -> snoc rs' (r + popCount w) >>= unsafeFreeze)
  {-# INLINE unsafeFreeze #-}
