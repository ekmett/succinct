{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

-- | @'_SUPERBLOCK_SIZE' = '_BLOCKSIZE' * '_BLOCKS_PER_SUPERBLOCK'@
_SUPERBLOCK_SIZE :: Int
_SUPERBLOCK_SIZE = 960 -- = lcm 64 15

-- | @ 7 <= '_BLOCK_SIZE' <= 15@
_BLOCK_SIZE :: Int
_BLOCK_SIZE = 15

_BLOCKS_PER_SUPERBLOCK :: Int
_BLOCKS_PER_SUPERBLOCK = 64

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
     = BOUNDS_CHECK(checkIndex) "RRR.!" i n $ case divMod i _BLOCK_SIZE of
       (q, r) -> case U.unsafeIndex cs q of
         0 -> False
         c | c' == _BLOCK_SIZE -> True
           | otherwise -> case div i _SUPERBLOCK_SIZE of
             q1 -> go (q1 * _BLOCKS_PER_SUPERBLOCK) $! P.unsafeIndex oos q1 where
               go !co !oo
                | co < q    = go (co + 1) (oo + lc)
                | otherwise = testBit (bitmap c' o) r
                where
                  lc = logBinomial _BLOCK_SIZE c'
                  lm = bit lc - 1
                  w = wd oo
                  b = bt oo
                  o = fromIntegral $
                      ( unsafeShiftR (P.unsafeIndex os w) b .|. if w == wd (oo + lc - 1) then 0 else
                        unsafeShiftL (P.unsafeIndex os $ w+1) (64-b)
                      ) .&. lm
           where c' = fromIntegral c

instance Dictionary Bool RRR where

instance Select0 RRR
instance Select1 RRR
instance Ranked RRR

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
  builder = Builder $ case vector :: Builder Int (P.Vector Int) of
    Builder (Building ki hi zi) -> case vector :: Builder Word4 (U.Vector Word4) of
      Builder (Building kc hc zc) -> case vector :: Builder Bit (U.Vector Bit) of
        Builder (Building ko ho zo) -> Building stop step start where

          start = BuildRRR 0 0 0 0
              <$> (zi >>= \zi' -> hi zi' 0)
              <*> (zi >>= \zi' -> hi zi' 0)
              <*> zc
              <*> zo

          step (BuildRRR n sb r o rs oos cs os) b
            | sub /= _BLOCK_SIZE - 1 = return $ BuildRRR n' sb' r' o rs oos cs os
            | otherwise = do
              let c = popCount sb'
                  delta = logBinomial _BLOCK_SIZE c
                  o' = o + delta
                  ofs = offset sb'
              os' <- foldlM (\osr oi -> ho osr $ Bit $ testBit ofs oi) os [0..delta-1]
              cs' <- hc cs (fromIntegral c :: Word4)
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

          stop (BuildRRR n sb r o rs oos cs os) = do
            let c     = popCount sb
                delta = logBinomial _BLOCK_SIZE c
                o'    = o + delta
                ofs   = offset sb
            rs'  <- hi rs r   >>= ki
            oos' <- hi oos o' >>= ki
            cs'  <- hc cs (fromIntegral c :: Word4) >>= kc
            V_Bit _ os' <- foldlM (\osr oi -> ho osr $ Bit $ testBit ofs oi) os [0..delta-1] >>= ko
            return $ RRR n rs' oos' cs' os'
