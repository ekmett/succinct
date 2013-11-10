{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Succinct.Dictionary.Class
  ( Access(..)
  , Dictionary(..)
  , Select0(..)
  , Select1(..)
  , Ranked(..)
  ) where

import Data.Bits
import Data.Word
import Succinct.Dictionary.Util.Broadword

class Access a t | t -> a where
  -- |
  -- @'size' t@ returns the total number of elements in @t@.
  size   :: t -> Int

  (!) :: t -> Int -> a
#ifndef HLINT
  default (!) :: (Dictionary a t, a ~ Bool) => t -> Int -> a
  (!) t i = rank True t i - rank True t (i - 1) == 1
#endif

instance Access a [a] where
  size = length
  {-# INLINE size #-}

  (!) = (!!)
  {-# INLINE (!) #-}

instance Access Bool Word64 where
  size _ = 64
  {-# INLINE size #-}

  (!) = testBit
  {-# INLINE (!) #-}

-- Succinct indexed dictionaries
--
-- @'select' a t@ is a right inverse of @'rank' a t@.
--
-- There is a Galois connection between @'rank' a t@ and @'select' a t'@.
--
-- @
-- 'select' a t ('rank' a t i) <= i
-- 'rank' a t ('select' a t i)  = i
-- @
class Access a t => Dictionary a t | t -> a where
  -- |
  -- @'rank' a t i@ returns the # of occurences of @a@ in @t@ up to position @i@
  -- for @0 < i <= 'size' t@
  rank   :: a -> t -> Int -> Int

  -- |
  -- @'select' a t i@ returns the position of the @i@th appearance of @a@ in @t@.
  -- as long as @0 < i <= 'rank' a t ('size' t)@
  select :: a -> t -> Int -> Int
  select a t i = search (\j -> rank a t j >= i) i (size t)
  {-# INLINE select #-}

-- For testing
instance Eq a => Dictionary a [a] where
  rank a xs0 n0 = go 0 n0 xs0 where
    go !acc 0 _       = acc
    go acc n (b:bs)
      | a == b    = go (acc + 1) (n-1) bs
      | otherwise = go acc       (n-1) bs
    go _ _ []  = error "rank []"
  {-# INLINE rank #-}

  select a xs0 n0 = go 0 n0 xs0 where
    go !acc 0 _ = acc
    go acc n (b:bs)
      | a == b    = go (acc + 1) (n-1) bs
      | otherwise = go (acc + 1) n     bs
    go _ _ [] = error "select []"
  {-# INLINE select #-}

-- | /O(1)/ 'rank' and 'select'
instance Dictionary Bool Word64 where
  rank True xs i
    | i >= 64   = popCount xs
    | otherwise = popCount $ xs .&. (bit i - 1)
  rank False xs i
    | i >= 64   = 64 - popCount xs
    | otherwise = i  - popCount (xs .&. (bit i - 1))
  {-# INLINE rank #-}

  select True  xs i = selectWord64 xs i
  select False xs i = selectWord64 (complement xs) i
  {-# INLINE select #-}

-- | Many structures that do not support arbitrary 'rank' can support a
-- limited notion of 'select'.
class Select0 t where
  select0 :: t -> Int -> Int
#ifndef HLINT
  default select0 :: Dictionary Bool t => t -> Int -> Int
  select0 = select False
#endif

-- | Many structures that do not support arbitrary 'rank' can support a
-- limited notion of 'select'.
class Select1 t where
  select1 :: t -> Int -> Int
#ifndef HLINT
  default select1 :: Dictionary Bool t => t -> Int -> Int
  select1 = select True
#endif

instance a ~ Bool => Select0 [a]
instance a ~ Bool => Select1 [a]

instance Select0 Word64 where
  select0 xs i = selectWord64 (complement xs) i
  {-# INLINE select0 #-}

instance Select1 Word64 where
  select1 xs i = selectWord64 xs i
  {-# INLINE select1 #-}

-- These are supplied as methods to avoid surprise regarding scoping on imports

-- | a classic bit-vector-based succinct indexed dictionary
--
-- For @1 <= i <= size t@:
--
-- @
-- 'rank0' t i + 'rank1' t i = i
-- @
--
-- Given @i = 'select1' t j@
--
-- @
-- 'rank0' t i = i - j
-- 'rank1' t i = j
-- @
--
-- Given @i = 'select0' t j@
--
-- @
-- 'rank0' t i = j
-- 'rank1' t i = i - j
-- @
--
class (Select0 t, Select1 t, Dictionary Bool t) => Ranked t where
  -- |
  -- @
  -- 'rank0' t i = i - 'rank1' t i
  -- 'rank0' = 'rank' 'False'
  -- @
  rank0 :: Ranked t => t -> Int -> Int
  rank0 = rank False

  -- |
  -- @
  -- 'rank1' t i = i - 'rank0' t i
  -- 'rank1' = 'rank' 'True'
  -- @
  rank1 :: Ranked t => t -> Int -> Int
  rank1 = rank True

  -- | @'rank_' t i@ return the number of bits to the left of position @i@
  --
  -- When @i > 1@:
  --
  -- @
  -- 'rank_' t i = 'rank1' t (i - 1)
  -- @
  --
  -- The result is @0@ otherwise.
  rank_ :: Ranked t => t -> Int -> Int
  rank_ _ 1 = 0
  rank_ t i = rank1 t (i - 1)

instance (Select0 t, Select1 t, Dictionary Bool t) => Ranked t

-- | Offset binary search
--
-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@
search :: (Int -> Bool) -> Int -> Int -> Int
search p = go where
  go l h
    | l == h    = l
    | p m       = go l m
    | otherwise = go (m+1) h
    where hml = h - l
          m = l + unsafeShiftR hml 1 + unsafeShiftR hml 6
{-# INLINE search #-}

