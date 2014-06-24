module Succinct.Internal.Broadword
  ( reverseBits
  , reverseBytes
  , byteSum
  , byteCounts
  , lsb, msb
  , leq8, uleq8, nonzero8
  , selectWord64
  ) where

import Control.Exception
import Data.Bits
import Data.Word
import Foreign.C.Types

-- | reverse the bits in a word
reverseBits :: Word64 -> Word64
reverseBits a = reverseBytes d where
  m1 = 0x5555555555555555
  m2 = 0x3333333333333333
  m3 = 0x0F0F0F0F0F0F0F0F
  b = shiftR a 1 .&. m1 .|. shiftL (a .&. m1) 1
  c = shiftR b 2 .&. m2 .|. shiftL (b .&. m2) 2
  d = shiftR c 4 .&. m3 .|. shiftL (c .&. m3) 4

foreign import ccall unsafe reverseBytes :: Word64 -> Word64
foreign import ccall unsafe bsf64 :: Word64 -> CULong
foreign import ccall unsafe bsr64 :: Word64 -> CULong

-- | Calculate the total of the unsigned bytes of a 64-bit word;
-- requires that the sum fits in a byte
byteSum :: Word64 -> Word64
byteSum a = shiftR (a * 0x0101010101010101) 56

-- | Convert a word of various bits into a word where each byte contains the count of bits in the corresponding original byte
--
-- @'popCount' = 'byteSum' . 'byteCounts'@
byteCounts :: Word64 -> Word64
byteCounts a = d .&. lsns where
  threes  = 0x3333333333333333
  as      = 0xAAAAAAAAAAAAAAAA
  lsbs    = 0x0101010101010101
  lsns = 0x0f0f * lsbs
  b = a - shiftR (a .&. as) 1
  c = (b .&. threes) + (shiftR b 2 .&. threes)
  d = c + shiftR c 4

-- | signed compare byte by byte, returning whether or not the result is less than or equal to
-- the corresponding byte in the other word as the least significant bit of each byte
leq8 :: Word64 -> Word64 -> Word64
leq8 x y = shiftR (w .&. msbs) 7 where
  msbs = 0x8080808080808080
  z = (y .|. msbs) - (x .&. complement msbs)
  w = x `xor` y `xor` z

-- | unsigned compare byte by byte, returning whether or not the result is less than or equal to
-- the corresponding byte in the other word as the least significant bit of each byte
uleq8 :: Word64 -> Word64 -> Word64
uleq8 x y = shiftR (w .&. msbs) 7 where
  msbs = 0x8080808080808080
  z = (y .|. msbs) - (x .&. complement msbs)
  w = x `xor` y `xor` z `xor` (x .&. complement y)

-- | For every byte in the input test whether it's non-zero, setting
-- the corresponding byte of the result to 0x01 or 0x00 accordingly
--
-- @'nonzero8' x = 'uleq8' 0x0101010101010101 x@
nonzero8 :: Word64 -> Word64
nonzero8 x = shiftR ((x .|. ((x .|. msbs) - lsbs)) .&. msbs) 7 where
  msbs = 0x8080808080808080
  lsbs = 0x0101010101010101

selectWord64 :: Word64 -> Int -> Int
selectWord64 x k = assert (k < popCount x) (place + offset) where
  wk      = fromIntegral k
  lsbs    = 0x0101010101010101
  hi      = 0xFFFFFFFFFFFFFFF8
  inc     = 0x8040201008040201
  sums    = byteCounts x * lsbs
  steps   = wk * lsbs
  place   = fromIntegral $ shiftR (leq8 sums steps * lsbs) 53 .&. hi
  br      = wk - (shiftR (shiftL sums 8) place .&. 0xFF)
  spread  = (shiftR x place .&. 0xFF) * lsbs
  bitSums = nonzero8 (spread .&. inc) * lsbs
  offset  = fromIntegral $ shiftR (leq8 bitSums (br * lsbs) * lsbs) 56

-- | return the position of the least significant set bit in a word, assumes that there is a set bit
lsb :: Word64 -> Int
lsb a = assert (a /= 0) $ fromIntegral $ bsf64 a

-- | return the position of the most significant set bit in a word, assumes that there is a set bit
msb :: Word64 -> Int
msb a = assert (a /= 0) $ fromIntegral $ bsr64 a
