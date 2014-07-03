{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash, UnliftedFFITypes #-}

module Succinct.Internal.PopCount
  ( popCountWord64
  , VectorInternal(..)
  , vectorToInternal
  , vectorFromInternal
  , popCountSlice
  , popCountBitSlice
  , popCount512Bits
  , unsafeIndexInternal
  ) where

import Control.Monad.ST
import Data.Function (on)
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

-- TODO(klao): get rid of this once P.Vector constructor is exposed
data VectorInternal =
  VectorInternal {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !ByteArray

instance Eq VectorInternal where
  (==) = (==) `on` vectorFromInternal

instance Ord VectorInternal where
  compare = compare `on` vectorFromInternal

instance Show VectorInternal where
  showsPrec p = showsPrec p . vectorFromInternal

vectorToInternal :: P.Vector Word64 -> VectorInternal
vectorToInternal v = runST $ do
  PM.MVector off len mba <- P.unsafeThaw v
  ba <- unsafeFreezeByteArray mba
  return $ VectorInternal off len ba
{-# INLINE vectorToInternal #-}

vectorFromInternal :: VectorInternal -> P.Vector Word64
vectorFromInternal (VectorInternal off len ba) = runST $ do
  mba <- unsafeThawByteArray ba
  P.unsafeFreeze $ PM.MVector off len mba
{-# INLINE vectorFromInternal #-}

unsafeIndexInternal :: VectorInternal -> Int -> Word64
unsafeIndexInternal (VectorInternal off _ ba) i = indexByteArray ba (off + i)
{-# INLINE unsafeIndexInternal #-}

-- TODO(klao): create an index-checking version.
popCountSlice :: VectorInternal -> Int -> Int -> Int
popCountSlice (VectorInternal voff _ (ByteArray ba)) off len
  = popCountNWords ba (voff + off) len
{-# INLINE popCountSlice #-}

popCountBitSlice :: VectorInternal -> Int -> Int -> Int
popCountBitSlice (VectorInternal voff _ (ByteArray ba)) off bitlen
  = popCountNBits ba (voff + off) bitlen
{-# INLINE popCountBitSlice #-}

popCount512Bits :: VectorInternal -> Int -> Int
popCount512Bits (VectorInternal voff _ (ByteArray ba)) off
  = popCount512 ba (voff + off)
{-# INLINE popCount512Bits #-}
