{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Succinct.Dictionary.Poppy
  ( Poppy(..)
  , poppy
  ) where

import Control.Applicative
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Primitive as P
import Data.Vector.Internal.Check as Ck
import Data.Word
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class
import Succinct.Internal.Bit as B
import Succinct.Internal.PopCount

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

data Poppy = Poppy
             {-# UNPACK #-} !Int             -- length in bit
             {-# UNPACK #-} !VectorInternal  -- the bitvector
             !(P.Vector Word64)              -- upper layer of the inventory
             !(P.Vector Word64)              -- main layer of the inventory
  deriving (Eq,Ord,Show)

instance Access Bool Poppy where
  size (Poppy n _ _ _) = n
  {-# INLINE size #-}

  (!) (Poppy n bs _ _) i
     = BOUNDS_CHECK(checkIndex) "Poppy.!" i n
     $ testBit (unsafeIndexInternal bs $ wd i) (bt i)
  {-# INLINE (!) #-}

instance Bitwise Poppy B.Vector where
  bitwise (Poppy n v _ _) = V_Bit n $ vectorFromInternal v
  {-# INLINE bitwise #-}

instance Dictionary Bool Poppy

instance Select0 Poppy
instance Select1 Poppy

instance Ranked Poppy where
  rank1 t@(Poppy n _ _ _) i =
    BOUNDS_CHECK(checkIndex) "rank" i (n+1) $
    unsafeRank1 t i
  {-# INLINE rank1 #-}

  unsafeRank1 (Poppy _ ws ups ps) i = fromIntegral result
    where
      upperBlock = i `shiftR` 32
      upperCnt = fromIntegral $ P.unsafeIndex ups upperBlock

      block = i `shiftR` 9

      -- TODO(klao): Is it better to read it as 2 Word32s?
      d4 = P.unsafeIndex ps (i `shiftR` 11)
      upto4BlockCnt = fromIntegral $ d4 `shiftR` 32
      k4 = block .&. 3
      m = d4 .&. (1 `unsafeShiftL` (10 * k4) - 1)
      blockCnts = fromIntegral $
        m .&. 1023 + (m `shiftR` 10) .&. 1023 + (m `shiftR` 20) .&. 1023

      blockStart = block `shiftL` 3
      bitInBlock = i .&. 511
      cnt = popCountBitSlice ws blockStart bitInBlock

      result = upto4BlockCnt + blockCnts + cnt + upperCnt
  {-# INLINE unsafeRank1 #-}

poppy :: Bitwise t B.Vector => t -> Poppy
poppy t = case bitwise t of
  V_Bit n ws0 -> Poppy n ws ups ps
    where
      ws = vectorToInternal ws0
      psSize = 1 + (n + 512) `shiftR` 11
      (ups, ps) = runST $ case poppyBlockBuilder $ vectorSized psSize of
        Builder (Building kp hp zp) -> zp >>= go 0 >>= kp
          where
            nWords = wd n
            go k s | k' <= nWords  = hp s (popCount512Bits ws k) >>= go k'
                   | otherwise     = hp s (popCountBitSlice ws k remBits)
              where
                remBits = n - k `shiftL` 6
                k' = k + 8
{-# INLINE [0] poppy #-}
{-# RULES "poppy" poppy = id #-}


data BuildPoppyBlock a b = BPB
  {-# UNPACK #-} !Int    -- block count
  {-# UNPACK #-} !Word64 -- full popcount
  {-# UNPACK #-} !Word64 -- lower popcount
  {-# UNPACK #-} !Word64 -- 4 block popcounts
  !a                     -- upper vector builder
  !b                     -- lower vector builder

poppyBlockBuilder :: Builder Word64 (P.Vector Word64)
                  -> Builder Int (P.Vector Word64, P.Vector Word64)
poppyBlockBuilder vectorBuilder = Builder $ case vector of
  Builder (Building ku hu zu) -> case vectorBuilder of
    Builder (Building kl hl zl) -> Building stop step start
      where
        uMask = 1 `shiftL` (32 - 9) - 1
        start = BPB 0 0 0 0 <$> (zu >>= (`hu` 0)) <*> zl
        step (BPB n pc lpc cw us ls) p
          | n .&. uMask == uMask  = BPB n' pc' 0 0 <$> hu us pc' <*> hl ls cw
          | n .&. 3 == 3          = BPB n' pc' lpc' cw'' us <$> hl ls cw
          | otherwise             = return $ BPB n' pc' lpc' cw' us ls
          where
            n' = n + 1
            pw = fromIntegral p
            pc' = pc + pw
            lpc' = lpc + pw
            cw'' = lpc' `shiftL` 32
            i = n .&. 3
            cw' = cw .|. (pw `unsafeShiftL` (i * 10))
        stop (BPB _n _pc _lpc cw us ls) = do
          ls' <- hl ls cw
          ups <- ku us
          ps <- kl ls'
          return (ups, ps)
{-# INLINE poppyBlockBuilder #-}

data BuildPoppyWords a b = BPW
  {-# UNPACK #-} !Int  -- word count
  {-# UNPACK #-} !Int  -- block popcount
  !a                   -- poppy block builder
  !b                   -- vector builder

poppyWordBuilder :: Builder Word64 Poppy
poppyWordBuilder = Builder $ case vector of
  Builder (Building kw hw zw) -> case poppyBlockBuilder vector of
    Builder (Building kb hb zb) -> Building stop step start
      where
        start = BPW 0 0 <$> zb <*> zw
        step (BPW n pc bs ws) w
          | n .&. 7 == 7  = BPW n' 0 <$> hb bs pc' <*> hw ws w
          | otherwise     = BPW n' pc' bs <$> hw ws w
          where
            n' = n + 1
            pc' = pc + popCountWord64 w
        stop (BPW n pc bs ws) = do
          bs' <- hb bs pc
          vec <- kw ws
          (ups, ps) <- kb bs'
          return $ Poppy (n * 64) (vectorToInternal vec) ups ps
{-# INLINE poppyWordBuilder #-}

instance Buildable Bool Poppy where
  builder = Builder $ case poppyWordBuilder of
    Builder pwb -> wordToBitBuilding pwb fixSize
      where
        fixSize n (Poppy _ ws ups ps) = return $ Poppy n ws ups ps
  {-# INLINE builder #-}
