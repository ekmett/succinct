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

data Build9 a b
  = Build9 {-# UNPACK #-} !Int    -- ^ bit position
           {-# UNPACK #-} !Word64 -- ^ current word
           {-# UNPACK #-} !Int    -- ^ current rank
           a                      -- ^ word builder
           b                      -- ^ rank builder

instance Buildable Bool Rank9 where
  builder = case vector of
    Builder kw hw zw -> case vector of
      Builder kr hr zr -> Builder stop step start
       where start = Build9 0 0 0 <$> zw <*> zr
             step (Build9 n w r ws rs) b
               | n63 == 63 = Build9 (n + 1) 0 (r + popCount w') <$> hw ws w' <*> hr rs r
               | otherwise = return $ Build9 (n + 1) w' r ws rs
               where w' = if b then setBit w n63 else w
                     n63 = n .&. 63
             stop (Build9 n w r ws rs)
               | n .&. 63 == 0 = Rank9 n <$> kw ws <*> (hr rs r >>= kr)
               | otherwise = Rank9 n
                         <$> (hw ws w >>= kw)
                         <*> (hr rs r >>= \rs' -> hr rs' (r + popCount w) >>= kr)
