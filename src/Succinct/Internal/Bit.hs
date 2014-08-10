{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Succinct.Internal.Bit
  ( Bit(..)
  , Decode64(..)
  , wds
  , wd
  , bt
  , unsafeBit
  , U.Vector(V_Bit)
  , UM.MVector(MV_Bit)
  , foldlMPadded
  , PackedBits(..)
  ) where

import Control.Monad
import Data.Bits
import Data.Int
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Word

wds :: Int -> Int
wds x = unsafeShiftR (x + 63) 6
{-# INLINE wds #-}

wd :: Int -> Int
wd x = unsafeShiftR x 6
{-# INLINE wd #-}

bt :: Int -> Int
bt x = x .&. 63
{-# INLINE bt #-}

-- The 'bit' from Data.Bits uses 'shiftL' thus introducing unnecessary branch.
--
-- Also 'bit' from Data.Bits doesn't inline properly on GHC 7.6.3.
unsafeBit :: Int -> Word64
unsafeBit i = 1 `unsafeShiftL` i
{-# INLINE unsafeBit #-}

class Decode64 t where
  -- @'decode' offset length v@ -- reads a variable length up to 64-bits long
  decode64 :: Int -> Int -> t -> Word64
#ifndef HLINT
  default decode64 :: (Integral t, Bits t) => Int -> Int -> t -> Word64
  decode64 oo lc os = fromIntegral $ (bit lc - 1) .&. shiftR os oo
#endif

instance Decode64 Word
instance Decode64 Word8
instance Decode64 Word16
instance Decode64 Word32
instance Decode64 Word64
instance Decode64 Int
instance Decode64 Int8
instance Decode64 Int16
instance Decode64 Int32
instance Decode64 Int64
instance Decode64 Integer

instance Decode64 (P.Vector Word64) where
  decode64 oo lc os = (bit lc - 1) .&.
    if b + lc < 64
    then unsafeShiftR (P.unsafeIndex os w) b
    else unsafeShiftR (P.unsafeIndex os w) b
     .|. unsafeShiftL (P.unsafeIndex os $ w+1) (64 - b)
    where w = wd oo
          b = bt oo

instance Decode64 (U.Vector Bit) where
  decode64 lc os (V_Bit _ bs) = decode64 lc os bs

newtype Bit = Bit Bool
 deriving (Show,Read,Eq,Ord,Enum,Bounded)

instance UM.Unbox Bit

data instance UM.MVector s Bit = MV_Bit {-# UNPACK #-} !Int !(PM.MVector s Word64)

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

data instance U.Vector Bit = V_Bit {-# UNPACK #-} !Int !(P.Vector Word64)
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

-- | Fold a Bit vector as a sequence of Word64s, with the last word
-- padded with 0 bits if incomplete.
foldlMPadded :: (G.Vector v Bit, G.Vector (Packed v) Word64, PackedBits v, Monad m)
             => (b -> Word64 -> m b) -> b -> v Bit -> m b
foldlMPadded f z v = go 0 z
  where
    n = G.length v
    ws = packedBits v
    k = wd (n - 1)
    t = bt n
    mask = fromIntegral $ (bit t - 1) + (t - 1) `shiftR` 63
    go !i !s | i < k     = f s (ws G.! i) >>= go (i+1)
             | i == k    = f s $ (ws G.! k) .&. mask
             | otherwise = return s

-- | Expose the underlying packed representation of a bit vector
class PackedBits v where
  type Packed v :: * -> *
  packedBits :: v Bit -> Packed v Word64

instance PackedBits U.Vector where
  type Packed U.Vector = P.Vector
  packedBits (V_Bit _ ws) = ws
