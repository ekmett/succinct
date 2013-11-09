{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Succinct.Tree.LOUDS
  ( LOUDS(..)
  , Tree(..)
  , fromTree
  ) where

import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9

newtype Tree = Node [Tree]
  deriving (Eq,Show)

-- | Jacobson's 1-based LOUDS
--
-- Visit every node in level order. Represent each node with @n@ children by @(1^n)0@.
--
-- We add an extra @10@ \"superroot\" to the top of the tree to avoid corner cases.
newtype LOUDS = LOUDS Rank9 deriving Show

instance Dictionary LOUDS where
  type Elem LOUDS = Bool
  size (LOUDS t) = size t
  rank a (LOUDS t) i = rank a t i
  (!) (LOUDS t) i = t ! i
  select a (LOUDS t) i = select a t i

fromTree :: Tree -> LOUDS
fromTree xs = LOUDS $ fromList $ True: False: go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)

level :: Int -> Tree -> [Bool] -> [Bool]
level 0 (Node cs) xs = replicate (length cs) True ++ (False:xs)
level n (Node cs) xs = foldr (level (n - 1)) xs cs
