{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
module Succinct.Tree.Binary
  (
  -- * Basic binary tree
    Binary(..)
  -- * Conversion
  , jacobson
  , fromBinary
  , toBinary
  , binary
  -- * Succinct binary tree zippers
  , Zipper(..)
  -- * Operations
  , root
  , top, parent
  , children
  , left
  , right
  , tip
  , bin
  ) where

import Control.Comonad
import Data.Foldable
import Data.Traversable
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9 as Rank9
import Succinct.Tree.Types

-- | Jacobson's encoding of a binary tree
jacobson :: Binary -> [Bool]
jacobson xs = go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)

fromBinary :: Binary -> Rank9
fromBinary = rank9 . jacobson

binary :: Ranked t => Zipper t -> Binary
binary t = case children t of
  Nothing    -> Tip
  Just (l,r) -> Bin (binary l) (binary r)

-- |
-- @
-- toTree (fromTree t) = t
-- @
toBinary :: Ranked t => t -> Binary
toBinary = binary . root

level :: Int -> Binary -> [Bool] -> [Bool]
level 0 (Bin _ _) xs = True :xs
level 0 Tip       xs = False:xs
level n (Bin l r) xs = level (n - 1) l $ level (n - 1) r xs
level _ Tip       xs = xs

-- |binary tree-zipper a la Jacobson
data Zipper t = Zipper {-# UNPACK #-} !Int t
  deriving (Eq,Show)

instance Functor Zipper where
  fmap f (Zipper i t) = Zipper i (f t)

instance Comonad Zipper where
  extract (Zipper _ t) = t
  duplicate w@(Zipper i _) = Zipper i w

instance Foldable Zipper where
  foldMap f (Zipper _ t) = f t

instance Traversable Zipper where
  traverse f (Zipper i t) = Zipper i <$> f t

-- | The @root@ of our tree is at position @1@
root :: t -> Zipper t
root = Zipper 1

-- | Is this node the 'root'?
top :: Zipper t -> Bool
top (Zipper i _) = i == 1

children :: Ranked t => Zipper t -> Maybe (Zipper t, Zipper t)
children (Zipper i t)
  | t ! i, j <- 2 * rank1 t i = Just (Zipper j t, Zipper (j + 1) t)
  | otherwise                 = Nothing

-- | @'parent' t i@ returns the parent for any node @i@ in a Binary tree,
-- other than the root. e.g. where @top i@ returns 'False'
parent :: Select1 t => Zipper t -> Zipper t
parent (Zipper i t) = Zipper (select1 t (div i 2)) t

-- | @'left' t i@ returns the left child of a node @i@ given @'bin' t i@
--
-- This is relatively unsafe. 'children' provides a safer access pattern
left :: Ranked t => Zipper t -> Zipper t
left (Zipper i t) = Zipper (2 * rank1 t i) t

-- | @right t i@ returns the right child of a node @i@ given @'bin' t i@
--
-- @
-- 'right' m i = 'left' m i + 1
-- @
--
-- This is relatively unsafe. 'children' providers a safer access pattern
right :: Ranked t => Zipper t -> Zipper t
right (Zipper i t) = Zipper (2 * rank1 t i + 1) t

-- | Is this node a 'Tip'?
tip :: Access Bool t => Zipper t -> Bool
tip (Zipper i t) = not (t ! i)

-- | Is this node a 'Bin'?
bin :: Access Bool t => Zipper t -> Bool
bin (Zipper i t) = t ! i

