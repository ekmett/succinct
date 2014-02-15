{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Raman, Raman, and Rao's succinct indexed dictionary
module Succinct.Dictionary.RRR
  ( RRR(..)
  ) where

import Control.Applicative
import Data.Bits
import Data.Foldable as F
import Data.Vector.Primitive as P
import Data.Vector.Unboxed as U
import Data.Word
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class
import Succinct.Internal.Bit
import Succinct.Internal.Word4 (Word4)
import Succinct.Internal.Binomial
import Data.Vector.Internal.Check as Ck

#define BOUNDS_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Bounds

-- |
-- >>> _SUPERBLOCK_SIZE == _BLOCKS_PER_SUPERBLOCK * _BLOCK_SIZE
-- True
_SUPERBLOCK_SIZE :: Int
_SUPERBLOCK_SIZE = 960 -- = lcm 64 15
{-# INLINE _SUPERBLOCK_SIZE #-}

-- |
-- >>> _BLOCK_SIZE == 15
-- True
_BLOCK_SIZE :: Int
_BLOCK_SIZE = 15
{-# INLINE _BLOCK_SIZE #-}

_BLOCKS_PER_SUPERBLOCK :: Int
_BLOCKS_PER_SUPERBLOCK = 64
{-# INLINE _BLOCKS_PER_SUPERBLOCK #-}

data RRR = RRR
  {-# UNPACK #-} !Int -- bit count
  !(P.Vector Int)     -- superblock ranks     -- one rank for every 960 bits (every 15 words)
  !(P.Vector Int)     -- offset offsets       -- one offset for every 960 bits of original data
  !(U.Vector Word4)   -- classes by sub-block -- one class for every 15 bits (64 per superblock)
  !(P.Vector Word64)  -- packed offsets       -- 64 per superblock, each 'logBinomial' class bits
  deriving Show

instance Access Bool RRR where
  size (RRR n _ _ _ _) = n

  (!) (RRR n _ oos cs os) i
     = BOUNDS_CHECK(checkIndex) "RRR.!" i n $ case div i _BLOCK_SIZE of
       q -> case U.unsafeIndex cs q of
         c0 | c0 == 0  -> False
            | c0 == 15 -> True
            | otherwise -> case div i _SUPERBLOCK_SIZE of
              q1 -> go (q1 * _BLOCKS_PER_SUPERBLOCK) $! P.unsafeIndex oos q1 where
                go !co !oo
                 | co < q    = go (co + 1) (oo + lc)
                 | otherwise = testBit (bitmap c $ fromIntegral $ decode64 oo lc os) $ mod i _BLOCK_SIZE
                 where c = fromIntegral (U.unsafeIndex cs co)
                       lc = logBinomial _BLOCK_SIZE c

instance Select0 RRR
instance Select1 RRR
instance Dictionary Bool RRR

instance Ranked RRR where
  rank1 (RRR n rs oos cs os) i
     = BOUNDS_CHECK(checkIndex) "RRR.!" i (n + 1)
     $ case divMod i _BLOCK_SIZE of
         (q,r) -> case div i (_SUPERBLOCK_SIZE) of
           q1 -> go (P.unsafeIndex rs q1) (q1 * _BLOCKS_PER_SUPERBLOCK) (P.unsafeIndex oos q1) where
             go !acc !co !oo
               | co < q    = go (acc + c) (co + 1) (oo + lc)
               | otherwise = acc + fromIntegral (popCount (bitmap c $ fromIntegral $ decode64 oo lc os) .&. (bit r - 1))
               where c = fromIntegral (U.unsafeIndex cs co)
                     lc = logBinomial _BLOCK_SIZE c
  rank0 rrr i = i - rank1 rrr i
  {-# INLINE rank0 #-}

data BuildRRR w x y z = BuildRRR
  {-# UNPACK #-} !Int    -- bits seen
  {-# UNPACK #-} !Word16 -- subblock accumulator
  {-# UNPACK #-} !Int    -- global rank
  {-# UNPACK #-} !Int    -- global offset
  w x y z -- super ranks, super offsets, classes by subblock, packed offsets

setBit' :: Bits a => a -> Int -> Bool -> a
setBit' a i True  = setBit a i
setBit' a _ False = a
{-# INLINE setBit' #-}

-- We could get much faster construction working a Word at a time.
instance Buildable Bool RRR where
  builder = Builder $ case vector of
    Builder (Building ki hi zi) -> case vector of
      Builder (Building kc hc zc) -> case vector of
        Builder (Building ko ho zo) -> Building stop step start where

          start = BuildRRR 0 0 0 0
              <$> (zi >>= \rs -> hi rs 0)   -- start at 0 rank
              <*> (zi >>= \oos -> hi oos 0) -- start at 0 offset
              <*> zc
              <*> zo

          step (BuildRRR n sb r o rs oos cs os) b
            | sub /= _BLOCK_SIZE - 1 = return $ BuildRRR n' sb' r' o rs oos cs os
            | otherwise = do
              let c = popCount sb'
                  delta = logBinomial _BLOCK_SIZE c
                  o' = o + delta
                  ofs = offset sb'
              os' <- foldlM (\osr -> ho osr . Bit . testBit ofs) os [0..delta-1]
              cs' <- hc cs (fromIntegral c)
              if mod n _SUPERBLOCK_SIZE /= _SUPERBLOCK_SIZE - 1
                then return $ BuildRRR n' 0 r' o' rs oos cs' os'
                else do
                  rs'  <- hi rs r'
                  oos' <- hi oos o'
                  return $ BuildRRR n' 0 r' o' rs' oos' cs' os'
            where sub = mod n _BLOCK_SIZE
                  sb' = setBit' sb sub b
                  n'  = n + 1
                  r'  = r + fromEnum b

          stop (BuildRRR n sb _r _o rs oos cs os) = do
            let c     = popCount sb
                delta = logBinomial _BLOCK_SIZE c
                ofs   = offset sb
            rs'  <- ki rs
            oos' <- ki oos
            cs'  <- hc cs (fromIntegral c) >>= kc
            V_Bit _ os' <- foldlM (\osr -> ho osr . Bit . testBit ofs) os [0..delta-1] >>= ko
            return $ RRR n rs' oos' cs' os'
  {-# INLINE builder #-}
