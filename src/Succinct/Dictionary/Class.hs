{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Succinct.Dictionary.Class
  ( Dictionary(..)
  , rank0, rank1, rank_
  , select0, select1
  ) where

import Data.Bits
import Data.Word
import Succinct.Dictionary.Broadword

-- Succinct indexed dictionaries
--
-- @'select' a t@ is a right inverse of @'rank' a t@.
--
-- There is a Galois connection between @'rank' a t@ and @'select' a t'@.
--
-- @
-- select a t (rank a t i) <= i
-- rank a t (select a t i)  = i
-- @
class Dictionary t where
  type Elem t :: *
  -- |
  -- @'size' t@ returns the total number of elements in @t@.

  size   :: t -> Int

  -- |
  -- @'rank' a t i@ returns the # of occurences of @a@ in @t@ up to position @i@
  -- for @0 < i <= 'size' t@
  rank   :: Elem t -> t -> Int -> Int

  -- |
  -- @'select' a t i@ returns the position of the @i@th appearance of @a@ in @t@.
  -- as long as @0 < i <= 'rank' a t ('size' t)@
  select :: Elem t -> t -> Int -> Int
  select a t i = search (\j -> rank a t j >= i) i (size t)
  {-# INLINE select #-}

  (!) :: t -> Int -> Elem t
#ifndef HLINT
  default (!) :: (Elem t ~ Bool) => t -> Int -> Elem t
  (!) t i = rank True t i - rank True t (i - 1) == 1
#endif

-- |
-- @
-- 'rank0' t i = i - 'rank1' t i
-- @
rank0 :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
rank0 = rank False

-- |
-- @
-- 'rank1' t i = i - 'rank0' t i
-- @
rank1 :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
rank1 = rank True

-- |
-- When @i = 'select0' t j@
--
-- @
-- 'rank0' t i = j
-- 'rank1' t i = i - j
-- @
select0 :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
select0 = select False

-- |
-- Given @i = 'select1' t j@
--
-- @
-- 'rank0' t i = i - j
-- 'rank1' t i = j
-- @
select1 :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
select1 = select True

-- | @rank_ t i@ return the number of bits to the left of position @i@
--
-- When @i > 1@:
--
-- @
-- rank_ t i = rank t (i - 1)
-- @
--
-- The result is 0 otherwise.
rank_ :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
rank_ _ 1 = 0
rank_ t i = rank1 t (i - 1)

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

-- For testing
instance Eq t => Dictionary [t] where
  type Elem [t] = t

  size = length
  {-# INLINE size #-}

  (!) = (!!)
  {-# INLINE (!) #-}

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
instance Dictionary Word64 where
  type Elem Word64 = Bool

  size _ = 64
  {-# INLINE size #-}

  (!) = testBit
  {-# INLINE (!) #-}

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
