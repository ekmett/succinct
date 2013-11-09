{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- | Jacobson's 1-based LOUDS
--
-- Visit every node in level order. Represent each node with @n@ children by @(1^n)0@.
--
-- We add an extra @10@ \"superroot\" to the top of the tree to avoid corner cases.
module Succinct.Tree.LOUDS
  ( fromTree
  , Tree(..)
  ) where

import Succinct.Dictionary.Rank9

newtype Tree = Node [Tree]
  deriving (Eq,Show)

-- | Convert a _finite_ Tree to Rank9
fromTree :: Tree -> Rank9
fromTree xs = fromList $ True: False: go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)

level :: Int -> Tree -> [Bool] -> [Bool]
level 0 (Node cs) xs = replicate (length cs) True ++ (False:xs)
level n (Node cs) xs = foldr (level (n - 1)) xs cs
