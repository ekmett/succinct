{-# LANGUAGE TypeFamilies #-}
module Succinct.Tree.Binary
  (
  -- * Succinct binary trees
    Tree(..)
  , Binary(..)
  , fromTree
  , toTree
  -- * Positions
  , root
  , parent
  , left
  , right
  , tip
  , bin
  , tree
  ) where

import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9 as Rank9

-- | A binary tree
data Tree = Bin Tree Tree | Tip deriving (Eq,Ord,Show)

-- | Jacobson's encoding of a succinct binary tree
newtype Binary = Binary Rank9
  deriving (Eq,Ord,Show)

fromTree :: Tree -> Binary
fromTree xs = Binary $ Rank9.fromList $ go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)

tree :: Binary -> Int -> Tree
tree j i
  | tip j i   = Tip
  | otherwise = Bin (tree j l) (tree j (l + 1))
  where l = left j i

-- |
-- @
-- toTree (fromTree t) = t
-- @
toTree :: Binary -> Tree
toTree t = tree t root

level :: Int -> Tree -> [Bool] -> [Bool]
level 0 (Bin _ _) xs = True :xs
level 0 Tip       xs = False:xs
level n (Bin l r) xs = level (n - 1) l $ level (n - 1) r xs
level _ Tip       xs = xs

instance Dictionary Binary where
  type Elem Binary = Bool
  size (Binary m) = size m
  rank a (Binary m) i = rank a m i
  select a (Binary m) i = select a m i

-- | The @root@ of our tree is at position @1@
root :: Int
root = 1

-- | Is this node a 'Tip'?
tip :: Binary -> Int -> Bool
tip m i = not (m ! i)

-- | Is this node a 'Bin'?
bin :: Binary -> Int -> Bool
bin = (!)

-- | @'parent' t i@ returns the parent for any node @i@ in a Binary tree,
-- such that @i /= root@
parent :: Binary -> Int -> Int
parent (Binary m) i = select True m (div i 2)

-- | @'left' t i@ returns the left child of a node @i@ given @'bin' t i@
left :: Binary -> Int -> Int
left (Binary m) i = 2 * rank True m i

-- | @right t i@ returns the right child of a node @i@ given @'bin' t i@
--
-- @
-- 'right' m i = 'left' m i + 1
-- @
right  :: Binary -> Int -> Int
right m i = left m i + 1
