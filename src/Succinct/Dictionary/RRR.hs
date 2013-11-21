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
import Succinct.Internal.Bit
import Succinct.Internal.Word4
import Succinct.Internal.Binomial

data RRR = RRR
  {-# UNPACK #-} !Int -- bit count
  !(P.Vector Int)     -- superblock ranks -- one rank for every 512 bits
  !(P.Vector Int)     -- offset offsets -- one offset for every 512 bits
  !(U.Vector Word4)   -- classes by sub-bock -- one class for every 15 bits out to 510 bits, the last 2 bits are just stored directly in the offset array.
  !(P.Vector Word64)  -- packed offsets: 34 per block w/ 2 bits between

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

instance Buildable Bool RRR where
  builder = Builder $ case vector :: Builder Int (P.Vector Int) of
    Builder (Building ki hi zi) -> case vector :: Builder Word4 (U.Vector Word4) of
      Builder (Building kc hc zc) -> case vector :: Builder Bit (U.Vector Bit) of
        Builder (Building ko ho zo) -> Building stop step start where
          start = BuildRRR 0 0 0 0 <$> (zi >>= \zi' -> hi zi' 0)
                                   <*> (zi >>= \zi' -> hi zi' 0)
                                   <*> zc
                                   <*> zo
          step (BuildRRR n sb r o rs oos cs os) b = case compare super 510 of
            LT
              | sub /= 14 -> return $ BuildRRR (n + 1) sb' (r + fromEnum b) o rs oos cs os
              | otherwise -> do -- emit sub block
                let c = popCount sb'
                let delta = logBinomial _BLOCK_SIZE c
                let o' = o + delta
                let ofs = offset sb'
                BuildRRR (n + 1) 0 (r + fromEnum b) o' rs oos
                  <$> hc cs (fromIntegral c :: Word4)
                  <*> foldlM (\osr oi -> ho osr $ Bit $ testBit ofs oi) os [0..delta-1]
              where sub = mod super _BLOCK_SIZE
                    sb' = setBit' sb sub b
            EQ -> BuildRRR (n + 1) sb (r + fromEnum b) (o + 1) rs oos cs <$> ho os (Bit b)
            GT -> do
              rs' <- hi rs r
              os' <- ho os (Bit b)
              return $ BuildRRR (n + 1) sb (r + fromEnum b) (o + 1) rs' oos cs os'
            where super = n .&. 511
          stop (BuildRRR n sb r _ rs oos cs os) = do
            let super = n .&. _SUPERBLOCK_MASK
                k = _BLOCK_SIZE - 1 - mod (n .&. _SUPERBLOCK_MASK) _BLOCK_SIZE
                c = popCount sb
                delta = logBinomial _BLOCK_SIZE c
                ofs = offset sb
            rs'  <- hi rs r >>= ki
            oos' <- ki oos
            cs'' <- hc cs (fromIntegral c) >>= \cs' -> foldlM (\csr _ -> hc csr 0) cs' [1..k-1] >>= kc -- -1 we're emitting
            os' <- if super < 510
                   then foldlM (\osr oi -> ho osr $ Bit $ testBit ofs oi) os [0..delta-1]
                   else return os
            os'' <- foldlM (\osr _ -> ho osr (Bit False)) os' [1.._EXCESS_BITS]
            V_Bit _ os''' <- ko os''
            return $ RRR n rs' oos' cs'' os'''

_SUPERBLOCK_MASK :: Int
_SUPERBLOCK_MASK = 511

_SUPERBLOCK_SIZE :: Int
_SUPERBLOCK_SIZE = 512

_BLOCK_SIZE :: Int
_BLOCK_SIZE = 15

_BLOCKS_PER_SUPERBLOCK :: Int
_BLOCKS_PER_SUPERBLOCK = 34

_EXCESS_BITS :: Int
_EXCESS_BITS = 2

{-
superblocks :: Int -> Int
superblocks n = unsafeShiftR (n + 511) 9

superblock :: Int -> Int
superblock n = unsafeShiftR n 9

subblock :: Int -> Int
subblock n = div (n .&. 511) 15
-}
