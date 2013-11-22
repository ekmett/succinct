{-# LANGUAGE RankNTypes #-}
module Succinct.Internal.Decoding
  ( Decoding(..)
  , runDecoding
  , decodeBinary
  , decodeUnary
  , decodeGamma
  , decodeDelta
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Vector.Primitive as P
import Succinct.Internal.Bit
import Succinct.Internal.Broadword


newtype Decoding a = Decoding
  { undecoding :: forall r. (a -> Int -> Word64 -> r) -> P.Vector Word64 -> Int -> Word64 -> r
  }

instance Functor Decoding where
  fmap f (Decoding m) = Decoding $ \k v i w -> m (k . f) v i w
  {-# INLINE fmap #-}

instance Applicative Decoding where
  pure a = Decoding $ \k _ i w -> k a i w
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Decoding where
  return a = Decoding $ \k _ i w -> k a i w
  {-# INLINE return #-}
  Decoding m >>= f = Decoding $ \k v -> m (\ a -> undecoding (f a) k v) v
  {-# INLINE (>>=) #-}

runDecoding :: Decoding a -> P.Vector Word64 -> Int -> (a, Int)
runDecoding (Decoding m) v i = m (\a i' _ -> (a,i')) v i (v P.! wd i)

-- read n bits
decodeBinary :: Int -> Decoding Word64
decodeBinary l = Decoding step where
  step k v i xs
    | b + l < 64
      = k ((bit l - 1) .&. unsafeShiftR xs b) (i + l) xs
    | ys <- P.unsafeIndex v (wd i + 1)
      = k ((bit l - 1) .&. (unsafeShiftR xs b .|. unsafeShiftL ys (64 - b))) (i + l) ys
    where b = bt i

decodeUnary :: Decoding Int
decodeUnary = Decoding step where
  step k v i xs
    | b == 0
      = gallop 0 k v (wd i)
    | u <- lsb (complement (unsafeShiftL xs b))
      = if u + b < 64
        then k u (i + u + 1) xs
        else gallop u k v (wd i + 1)
    where b = bt i
  gallop acc k v wi = case P.unsafeIndex v wi of
    xs | xs == -1                 -> gallop (acc + 64) k v (wi + 1)
       | u <- lsb (complement xs) -> k (acc + u) (unsafeShiftL wi 6 + u) xs

-- | Elias gamma decoding
decodeGamma :: Decoding Word64
decodeGamma = do
  u <- decodeUnary
  b <- decodeBinary u
  return $! setBit b u

-- | Elias delta decoding
decodeDelta :: Decoding Word64
decodeDelta = do
  g <- fromIntegral <$> decodeGamma
  b <- decodeBinary g
  return $! setBit b g
