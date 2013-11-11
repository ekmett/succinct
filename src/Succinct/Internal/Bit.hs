{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy #-}
module Succinct.Internal.Bit
  ( Bit(..)
  , wds
  , wd
  , bt
  , U.Vector(V_Bit)
  , UM.MVector(MV_Bit)
  ) where

import Control.Monad
import Data.Bits
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
