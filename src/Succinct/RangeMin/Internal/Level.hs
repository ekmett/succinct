{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Succinct.RangeMin.Internal.Level
  ( Level(..)
  , stride
  , levels
  ) where

import Succinct.Dictionary.Class
import Succinct.RangeMin.Internal.Delta
import Data.Bits
import Data.Int
import Data.Semigroup
import Data.Vector as V
import Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Base as UB
import Data.Vector.Primitive as P
import Data.Word

-- leaving n-minima unpacked deliberately
data Level
  = L8  {-# UNPACK #-} !Int {-# UNPACK #-} !(P.Vector Int8)  {-# UNPACK #-} !(P.Vector Int8)  !(P.Vector Int8)
  | L16 {-# UNPACK #-} !Int {-# UNPACK #-} !(P.Vector Int16) {-# UNPACK #-} !(P.Vector Int16) !(P.Vector Int16)
  | L64 {-# UNPACK #-} !Int {-# UNPACK #-} !(P.Vector Int64) {-# UNPACK #-} !(P.Vector Int64) !(P.Vector Int64)
  deriving Show

stride :: Level -> Int
stride (L8  s _ _ _) = s
stride (L16 s _ _ _) = s
stride (L64 s _ _ _) = s
{-# INLINE stride #-}

instance Access Delta Level where
  size (L8  _ es _ _) = P.length es
  size (L16 _ es _ _) = P.length es
  size (L64 _ es _ _) = P.length es
  {-# INLINE size #-}

  (!) (L8  _ es ms ns) i = Delta (fromIntegral (es P.! i)) (fromIntegral (ms P.! i)) (fromIntegral (ns P.! i))
  (!) (L16 _ es ms ns) i = Delta (fromIntegral (es P.! i)) (fromIntegral (ms P.! i)) (fromIntegral (ns P.! i))
  (!) (L64 _ es ms ns) i = Delta (fromIntegral (es P.! i)) (fromIntegral (ms P.! i)) (fromIntegral (ns P.! i))
  {-# INLINE (!) #-}

levelN :: Int -> Int -> [Delta] -> Level
levelN s n xs
  | s <= 64    = case U.fromListN n (fmap s8 xs)  of UB.V_3 _ (V_Int8 es)  (V_Int8 ms)  (V_Int8 ns)  -> L8  s es ms ns
  | s <= 16384 = case U.fromListN n (fmap s16 xs) of UB.V_3 _ (V_Int16 es) (V_Int16 ms) (V_Int16 ns) -> L16 s es ms ns
  | otherwise  = case U.fromListN n (fmap s64 xs) of UB.V_3 _ (V_Int64 es) (V_Int64 ms) (V_Int64 ns) -> L64 s es ms ns
{-# INLINE levelN #-}

s8 :: Delta -> (Int8, Int8, Int8)
s8 (Delta e m n) = (fromIntegral e, fromIntegral m, fromIntegral n)
{-# INLINE s8 #-}

s16 :: Delta -> (Int16,Int16,Int16)
s16 (Delta e m n) = (fromIntegral e, fromIntegral m, fromIntegral n)
{-# INLINE s16 #-}

s64 :: Delta -> (Int64,Int64,Int64)
s64 (Delta e m n) = (fromIntegral e, fromIntegral m, fromIntegral n)
{-# INLINE s64 #-}

levels :: P.Vector Word64 -> V.Vector Level
levels v = V.fromList $ Prelude.reverse $ go 16 (P.length v * 4)
  [ bits s | w <- P.toList v
           , s <- [ fromIntegral w :: Word16
                  , fromIntegral (unsafeShiftR w 16)
                  , fromIntegral (unsafeShiftR w 32)
                  , fromIntegral (unsafeShiftR w 48) ] ] where
  go !_ 0 !_ = []
  go s  1 xs = [levelN s 1 xs]
  go s  n xs = case quotRem n 2 of
    (q,r) -> levelN s n xs : go (s+s) (q + r) (paired xs)

paired :: [Delta] -> [Delta]
paired (x:y:xs) = x<>y : paired xs
paired [x]      = [x<>x] -- use garbage padding to preserve invariants
paired []       = []
