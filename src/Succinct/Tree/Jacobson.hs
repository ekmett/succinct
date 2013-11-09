{-# LANGUAGE TypeFamilies #-}
module Succinct.Tree.Jacobson
  ( Tree(..)
  , Jacobson(..)
  , fromTree
  , toTree
  -- * Positions
  , root
  , parent
  , left
  , right
  , tip
  , bin
  ) where

import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9 as Rank9

-- | A binary tree
data Tree = Bin Tree Tree | Tip deriving (Eq,Ord,Show)

-- | Jacobson's encoding of a succinct binary tree
newtype Jacobson = Jacobson Rank9
  deriving (Eq,Ord,Show)

fromTree :: Tree -> Jacobson
fromTree xs = Jacobson $ Rank9.fromList $ go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)

-- |
-- @
-- toTree (fromTree t) root = t
-- @
toTree :: Jacobson -> Int -> Tree
toTree j i
  | tip j i = Tip
  | otherwise = Bin (toTree j l) (toTree j (l + 1))
  where l = left j i

level :: Int -> Tree -> [Bool] -> [Bool]
level 0 (Bin _ _) xs = True :xs
level 0 Tip       xs = False:xs
level n (Bin l r) xs = level (n - 1) l $ level (n - 1) r xs
level _ Tip       xs = xs

instance Dictionary Jacobson where
  type Elem Jacobson = Bool
  size (Jacobson m) = size m
  rank a (Jacobson m) i = rank a m i
  select a (Jacobson m) i = select a m i

-- | The @root@ of our tree is at position @1@
root :: Int
root = 1

-- | Is this node a 'Tip'?
tip :: Jacobson -> Int -> Bool
tip m i = not (m ! i)

-- | Is this node a 'Bin'?
bin :: Jacobson -> Int -> Bool
bin = (!)

-- | @'parent' t i@ returns the parent for any node @i@ in a Jacobson tree,
-- such that @i /= root@
parent :: Jacobson -> Int -> Int
parent (Jacobson m) i = select True m (div i 2)

-- | @'left' t i@ returns the left child of a node @i@ given @'bin' t i@
left   :: Jacobson -> Int -> Int
left (Jacobson m) i = 2 * rank True m i

-- | @right t i@ returns the right child of a node @i@ given @'bin' t i@
--
-- @
-- 'right' m i = 'left' m i + 1
-- @
right  :: Jacobson -> Int -> Int
right m i = left m i + 1
