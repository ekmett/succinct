{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Succinct.Internal.StorableBitVector
  ( Vector(SV_Bit)
  , MVector(SMV_Bit)
  ) where
 
import Data.Bits
import Data.Word
import Control.Monad (liftM)

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Storable as S

import Succinct.Internal.Bit hiding (MVector, Vector)

type instance G.Mutable Vector = MVector

data MVector s a = SMV_Bit {-# UNPACK #-} !Int !(S.MVector s Word64)
        
instance GM.MVector MVector Bit where
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
  basicLength (SMV_Bit n _) = n
  basicUnsafeSlice i n (SMV_Bit _ u) = SMV_Bit n $ GM.basicUnsafeSlice i (wds n) u
  basicOverlaps (SMV_Bit _ v1) (SMV_Bit _ v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = do
    v <- GM.basicUnsafeNew (wds n)
    return $ SMV_Bit n v
  basicUnsafeReplicate n (Bit b) = do
    v <- GM.basicUnsafeReplicate (wds n) (if b then -1 else 0)
    return $ SMV_Bit n v
  basicUnsafeRead (SMV_Bit _ u) i = do
    w <- GM.basicUnsafeRead u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeWrite (SMV_Bit _ u) i (Bit b) = do
    let wn = wd i
    w <- GM.basicUnsafeRead u wn
    GM.basicUnsafeWrite u wn $ if b then setBit w (bt i) else clearBit w (bt i)
  basicClear (SMV_Bit _ u) = GM.basicClear u
  basicSet (SMV_Bit _ u) (Bit b) = GM.basicSet u $ if b then -1 else 0
  basicUnsafeCopy (SMV_Bit _ u1) (SMV_Bit _ u2) = GM.basicUnsafeCopy u1 u2
  basicUnsafeMove (SMV_Bit _ u1) (SMV_Bit _ u2) = GM.basicUnsafeMove u1 u2
  basicUnsafeGrow (SMV_Bit _ u) n = liftM (SMV_Bit n) (GM.basicUnsafeGrow u (wds n))

data Vector a = SV_Bit {-# UNPACK #-} !Int !(S.Vector Word64)

instance G.Vector Vector Bit where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicLength (SV_Bit n _)          = n
  basicUnsafeFreeze (SMV_Bit n u)   = liftM (SV_Bit n) (G.basicUnsafeFreeze u)
  basicUnsafeThaw (SV_Bit n u)      = liftM (SMV_Bit n) (G.basicUnsafeThaw u)
  basicUnsafeSlice i n (SV_Bit _ u) = SV_Bit n (G.basicUnsafeSlice i (wds n) u)
  basicUnsafeIndexM (SV_Bit _ u) i  = do
    w <- G.basicUnsafeIndexM u (wd i)
    return $ Bit $ testBit w (bt i)
  basicUnsafeCopy (SMV_Bit _ mu) (SV_Bit _ u) = G.basicUnsafeCopy mu u
  elemseq _ b z = b `seq` z

instance PackedBits Vector where
  type Packed Vector = S.Vector
  packedBits (SV_Bit _ ws) = ws
