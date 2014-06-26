{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash, UnliftedFFITypes #-}

module Succinct.Internal.PopCount
  ( popCountWord64
  , popCountVecSlice
  , popCountVec
  , popCountVecBitSlice
  , popCount512Bits
  ) where

import Control.Monad.ST
import Data.Primitive.ByteArray
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import Data.Word

-- TODO(klao): it would be a little bit faster to import this as
-- 'prim', cause then the code could literally be two processor
-- instructions:
--   __asm__("popcnt %%rbx, %%rbx\n\t" "jmp * (%%rbp)\n\t");
--
-- But, there's still some inefficiency around it (GHC saves the
-- registers before jumping), so the gain is less than it could be.
--
-- Also, foreign import prim kills the ghci for some reason, which
-- makes development a hassle, so I stick with the simple solution for
-- now.
foreign import ccall unsafe "popcnt_w64" popCountWord64 :: Word64 -> Int

foreign import ccall unsafe "popcnt_words" popCountNWords :: ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe "popcnt_bits" popCountNBits :: ByteArray# -> Int -> Int -> Int
foreign import ccall unsafe "popcnt_512" popCount512 :: ByteArray# -> Int -> Int

-- TODO(klao): create an index-checking version.
popCountVecSlice :: P.Vector Word64 -> Int -> Int -> Int
popCountVecSlice v off len = runST $ do
  PM.MVector voff _vlen mba <- P.unsafeThaw v
  ByteArray ba <- unsafeFreezeByteArray mba
  return $ popCountNWords ba (voff + off) len
{-# INLINE popCountVecSlice #-}

popCountVec :: P.Vector Word64 -> Int
popCountVec v = popCountVecSlice v 0 (P.length v)
{-# INLINE popCountVec #-}

popCountVecBitSlice :: P.Vector Word64 -> Int -> Int -> Int
popCountVecBitSlice v off bitlen = runST $ do
  PM.MVector voff _vlen mba <- P.unsafeThaw v
  ByteArray ba <- unsafeFreezeByteArray mba
  return $ popCountNBits ba (voff + off) bitlen
{-# INLINE popCountVecBitSlice #-}

popCount512Bits :: P.Vector Word64 -> Int -> Int
popCount512Bits v off = runST $ do
  PM.MVector voff _vlen mba <- P.unsafeThaw v
  ByteArray ba <- unsafeFreezeByteArray mba
  return $ popCount512 ba (voff + off)
{-# INLINE popCount512Bits #-}
