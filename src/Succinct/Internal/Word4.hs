{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Succinct.Internal.Word4
  ( Word4(..)
  , UM.MVector(MV_Word4)
  , U.Vector(V_Word4)
  , ny
  , wds
  , wd
  ) where

import Control.Monad
import Data.Bits
import Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as GM
import Data.Vector.Primitive as P
import Data.Vector.Primitive.Mutable as PM
import Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable as UM
import Data.Word

newtype Word4 = Word4 Word8 deriving (Eq,Ord)

instance Show Word4 where
  showsPrec d (Word4 n) = Prelude.showsPrec d n

instance Read Word4 where
  readsPrec d r = [ (Word4 n, r') | (n,r') <- readsPrec d r, n <= 15 ]

instance Num Word4 where
  Word4 n + Word4 m = Word4 $ (n + m) .&. 15
  Word4 n - Word4 m = Word4 $ (n - m) .&. 15
  Word4 n * Word4 m = Word4 $ (n * m) .&. 15
  fromInteger n = Word4 (fromInteger n .&. 15)
  abs n = n
  signum (Word4 n) = Word4 $ signum n

instance Bits Word4 where
  Word4 n .&. Word4 m = Word4 (n .&. m)
  Word4 n .|. Word4 m = Word4 (n .|. m)
  xor (Word4 n) (Word4 m) = Word4 (xor n m .&. 15)
  complement (Word4 n) = Word4 (complement n .&. 15)
  shift (Word4 n) i = Word4 (shift n i .&. 15)
  shiftL (Word4 n) i = Word4 (shiftL n i .&. 15)
  shiftR (Word4 n) i = Word4 (shiftR n i .&. 15)
  unsafeShiftL (Word4 n) i = Word4 (unsafeShiftL n i .&. 15)
  unsafeShiftR (Word4 n) i = Word4 (unsafeShiftR n i .&. 15)
  rotate (Word4 n) i  = Word4 $ (unsafeShiftL n (i .&. 3) .|. unsafeShiftR n (negate i .&. 3)) .&. 15
  rotateL (Word4 n) i = Word4 $ (unsafeShiftL n (i .&. 3) .|. unsafeShiftR n (negate i .&. 3)) .&. 15
  rotateR (Word4 n) i = Word4 $ (unsafeShiftL n (negate i .&. 3) .|. unsafeShiftR n (i .&. 3)) .&. 15
  bit i = Word4 (bit i .&. 15)
  setBit (Word4 n) i = Word4 (setBit n i .&. 15)
  clearBit (Word4 n) i = Word4 (clearBit n i .&. 15)
  complementBit (Word4 n) i = Word4 (complementBit n i .&. 15)
  testBit (Word4 n) i = testBit n i
  bitSize _ = 4
  isSigned _ = False
  popCount (Word4 n) = popCount n
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
  bitSizeMaybe _ = Just 4
#endif


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
instance FiniteBits Word4 where
  finiteBitSize _ = 4
#endif

instance Real Word4 where
  toRational (Word4 n) = toRational n

instance Integral Word4 where
  quot (Word4 m) (Word4 n) = Word4 (quot m n .&. 15)
  rem (Word4 m) (Word4 n) = Word4 (rem m n .&. 15)
  div (Word4 m) (Word4 n) = Word4 (div m n .&. 15)
  mod (Word4 m) (Word4 n) = Word4 (mod m n .&. 15)
  quotRem (Word4 m) (Word4 n) = case quotRem m n of
    (q, r) -> (Word4 $ q .&. 15, Word4 $ r .&. 15)
  divMod (Word4 m) (Word4 n) = case divMod m n of
    (q, r) -> (Word4 $ q .&. 15, Word4 $ r .&. 15)
  toInteger (Word4 n) = toInteger n

instance Bounded Word4 where
  minBound = Word4 0
  maxBound = Word4 15

instance Enum Word4 where
  succ (Word4 n)
    | n == 15 = error "Prelude.Enum.succ{Word4}: tried to take `succ' of maxBound"
    | otherwise = Word4 (n + 1)
  pred (Word4 n)
    | n == 0 = error "Prelude.Enum.pred{Word4}: tried to take `pred' of minBound"
    | otherwise = Word4 (n - 1)
  toEnum n
    | n == n .&. 15 = Word4 (fromIntegral n)
    | otherwise = error $ "Enum.toEnum{Word4}: tag (" Prelude.++ show n Prelude.++ ") is outside of bounds (0,15)"
  fromEnum (Word4 n) = fromEnum n

instance UM.Unbox Word4

ny :: Int -> Int
ny x = x .&. 15
{-# INLINE ny #-}

wd :: Int -> Int
wd x = unsafeShiftR x 4
{-# INLINE wd #-}

wds :: Int -> Int
wds x = unsafeShiftR (x + 15) 4
{-# INLINE wds #-}

getWord4 :: Word64 -> Int -> Word4
getWord4 w n = Word4 $ fromIntegral (unsafeShiftR w (4*n) .&. 0xf)
{-# INLINE getWord4 #-}

setWord4 :: Word64 -> Int -> Word4 -> Word64
setWord4 w n (Word4 w4) = w .&. complement (unsafeShiftL 0xf n4) .|. unsafeShiftL (fromIntegral w4) n4
  where !n4 = 4*n
{-# INLINE setWord4 #-}

data instance UM.MVector s Word4 = MV_Word4 {-# UNPACK #-} !Int !(PM.MVector s Word64)

tile :: Word4 -> Word64
tile (Word4 b) = b4
  where !b0 = fromIntegral b
        !b1 = b0 .|. unsafeShiftL b0 4
        !b2 = b1 .|. unsafeShiftL b1 8
        !b3 = b2 .|. unsafeShiftL b2 16
        !b4 = b3 .|. unsafeShiftL b3 32

instance GM.MVector U.MVector Word4 where
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
  basicLength (MV_Word4 n _) = n
  basicUnsafeSlice i n (MV_Word4 _ u) = MV_Word4 n $ GM.basicUnsafeSlice i (wds n) u
  basicOverlaps (MV_Word4 _ v1) (MV_Word4 _ v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = do
    v <- GM.basicUnsafeNew (wds n)
    return $ MV_Word4 n v
  basicUnsafeReplicate n w4 = do
    v <- GM.basicUnsafeReplicate (wds n) (tile w4)
    return $ MV_Word4 n v
  basicUnsafeRead (MV_Word4 _ u) i = do
    w <- GM.basicUnsafeRead u (wd i)
    return $ getWord4 w (ny i)
  basicUnsafeWrite (MV_Word4 _ u) i w4 = do
    let wn = wd i
    w <- GM.basicUnsafeRead u wn
    GM.basicUnsafeWrite u wn (setWord4 w (ny i) w4)
  basicClear (MV_Word4 _ u) = GM.basicClear u
  basicSet (MV_Word4 _ u) w4 = GM.basicSet u (tile w4)
  basicUnsafeCopy (MV_Word4 _ u1) (MV_Word4 _ u2) = GM.basicUnsafeCopy u1 u2
  basicUnsafeMove (MV_Word4 _ u1) (MV_Word4 _ u2) = GM.basicUnsafeMove u1 u2
  basicUnsafeGrow (MV_Word4 _ u) n = liftM (MV_Word4 n) (GM.basicUnsafeGrow u (wds n))

data instance U.Vector Word4 = V_Word4 {-# UNPACK #-} !Int !(P.Vector Word64)

instance G.Vector U.Vector Word4 where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicLength (V_Word4 n _)          = n
  basicUnsafeFreeze (MV_Word4 n u)   = liftM (V_Word4 n) (G.basicUnsafeFreeze u)
  basicUnsafeThaw (V_Word4 n u)      = liftM (MV_Word4 n) (G.basicUnsafeThaw u)
  basicUnsafeSlice i n (V_Word4 _ u) = V_Word4 n (G.basicUnsafeSlice i (wds n) u)
  basicUnsafeIndexM (V_Word4 _ u) i  = do
    w <- G.basicUnsafeIndexM u (wd i)
    return $ getWord4 w (ny i)
  basicUnsafeCopy (MV_Word4 _ mu) (V_Word4 _ u) = G.basicUnsafeCopy mu u
  elemseq _ b z = b `seq` z
