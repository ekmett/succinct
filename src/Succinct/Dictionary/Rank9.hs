{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
module Succinct.Dictionary.Rank9
  ( Rank9(..)
  , fromList
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Internal.Check as Ck
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word
import qualified Succinct.Dictionary.Class as Succinct

#define BOUNDS_CHECK(f) (Ck.f __FILE__ __LINE__ Ck.Bounds)

data Rank9 = Rank9 {-# UNPACK #-} !Int !(U.Vector Word64) !(U.Vector Int)
  deriving (Eq,Ord,Show)

instance Succinct.Access Bool Rank9 where
  size (Rank9 n _ _) = n
  {-# INLINE size #-}
instance Succinct.Dictionary Bool Rank9 where
  rank True xs n = rank1 xs n
  rank False xs n = n - rank1 xs n
  {-# INLINE rank #-}

instance Succinct.Select0 Rank9
instance Succinct.Select1 Rank9

rank1 :: Rank9 -> Int -> Int
rank1 (Rank9 n ws ps) i
  = BOUNDS_CHECK(checkIndex) "rank" i (n+1)
  $ (ps U.! w) + popCount ((ws U.! w) .&. (bit (bt i) - 1))
  where w = wd i

wds :: Int -> Int
wds x = unsafeShiftR (x + 63) 6
{-# INLINE wds #-}

wd :: Int -> Int
wd x = unsafeShiftR x 6
{-# INLINE wd #-}

bt :: Int -> Int
bt x = x .&. 63
{-# INLINE bt #-}

fromList :: [Bool] -> Rank9
fromList xs = case U.fromList (Bit <$> xs) of
  V_Bit n v -> Rank9 n v $ U.scanl (\a b -> a + popCount b) 0 v
{-# INLINE fromList #-}

newtype Bit = Bit Bool
 deriving (Show,Read,Eq,Ord,Enum,Bounded)

instance UM.Unbox Bit

data instance UM.MVector s Bit = MV_Bit {-# UNPACK #-} !Int !(UM.MVector s Word64)

instance GM.MVector U.MVector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bit n _) = n
  basicUnsafeSlice i n (MV_Bit _ u) = MV_Bit n $ GM.basicUnsafeSlice i (wds n) u
  basicOverlaps (MV_Bit _ v1) (MV_Bit _ v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = do
    v <- GM.basicUnsafeNew (wds n)
    return $ MV_Bit n v
  basicUnsafeReplicate n (Bit b) = do
    v <- GM.basicUnsafeReplicate (wds n) (if b then -1 else 0)
    return $ MV_Bit n v
  basicUnsafeRead (MV_Bit _ u) i = do
    w <- GM.basicUnsafeRead u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeWrite (MV_Bit _ u) i (Bit b) = do
    let wn = wd i
    w <- GM.basicUnsafeRead u wn
    GM.basicUnsafeWrite u wn $ if b then setBit w (bt i) else clearBit w (bt i)
  basicClear (MV_Bit _ u) = GM.basicClear u
  basicSet (MV_Bit _ u) (Bit b) = GM.basicSet u $ if b then -1 else 0
  basicUnsafeCopy (MV_Bit _ u1) (MV_Bit _ u2) = GM.basicUnsafeCopy u1 u2
  basicUnsafeMove (MV_Bit _ u1) (MV_Bit _ u2) = GM.basicUnsafeMove u1 u2
  basicUnsafeGrow (MV_Bit _ u) n = liftM (MV_Bit n) (GM.basicUnsafeGrow u (wds n))

data instance U.Vector Bit = V_Bit {-# UNPACK #-} !Int !(U.Vector Word64)
instance G.Vector U.Vector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicLength (V_Bit n _)          = n
  basicUnsafeFreeze (MV_Bit n u)   = liftM (V_Bit n) (G.basicUnsafeFreeze u)
  basicUnsafeThaw (V_Bit n u)      = liftM (MV_Bit n) (G.basicUnsafeThaw u)
  basicUnsafeSlice i n (V_Bit _ u) = V_Bit n (G.basicUnsafeSlice i (wds n) u)
  basicUnsafeIndexM (V_Bit _ u) i  = do
    w <- G.basicUnsafeIndexM u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeCopy (MV_Bit _ mu) (V_Bit _ u) = G.basicUnsafeCopy mu u
  elemseq _ b z = b `seq` z
