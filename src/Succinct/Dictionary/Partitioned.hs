module Succinct.Dictionary.Partitioned
  ( Partitioned(..)
  , fromListWith
  ) where

import Control.Applicative
import Control.Arrow
import Data.Foldable
import Data.List (group)
import Data.Monoid
import Data.Traversable
import Succinct.Dictionary.Class

-- | A partitioned succinct dictionary only supports 'select0' and 'select1' operations
--
-- This is sufficient to implement LOUDS with double-indexing
data Partitioned t = PE | P0 t t | P1 t t
  deriving (Eq,Ord,Show)

instance Functor Partitioned where
  fmap _ PE = PE
  fmap f (P0 r0 r1) = P0 (f r0) (f r1)
  fmap f (P1 r0 r1) = P1 (f r0) (f r1)

instance Foldable Partitioned where
  foldMap _ PE = mempty
  foldMap f (P0 r0 r1) = f r0 `mappend` f r1
  foldMap f (P1 r0 r1) = f r0 `mappend` f r1

instance Traversable Partitioned where
  traverse _ PE = pure PE
  traverse f (P0 r0 r1) = P0 <$> f r0 <*> f r1
  traverse f (P1 r0 r1) = P1 <$> f r0 <*> f r1

instance Ranked t => Select0 (Partitioned t) where
  select0 PE _ = error "Partitioned.select0: empty"
  select0 (P0 r0 r1) i = select1 r1 (rank_ r0 i) + i
  select0 (P1 r0 r1) i = select1 r1 (rank_ r0 i + 1) + i

instance Ranked t => Select1 (Partitioned t) where
  select1 PE _ = error "Partitioned.select1: empty"
  select1 (P0 r0 r1) i = select1 r0 (rank_ r1 i + 1) + i
  select1 (P1 r0 r1) i = select1 r0 (rank_ r1 i) + i

fromListWith :: ([Bool] -> r) -> [Bool] -> Partitioned r
fromListWith _ [] = PE
fromListWith f t  = pn (f zeroes) (f ones)
  where
    runs = (head &&& length) <$> group t
    zeroes = Prelude.foldr (count not) [] runs
    ones = Prelude.foldr (count id) [] runs
    count p (x,n) xs
      | p x       = replicate (n - 1) False ++ (True : xs)
      | otherwise = xs
    pn | head t    = P1
       | otherwise = P0
