{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
module Succinct.Tree.LOUDS.Single
  (
  -- * Single indexing
    toTree
  , root
  , parent
  , children
  , next
  -- * LOUDS tree representaton
  , module Succinct.Tree.LOUDS
  ) where

import Control.Applicative
import Succinct.Dictionary.Class
import Succinct.Tree.LOUDS

-- | single indexing a la Jacobson

-- | The 'root' of our succinct tree.
root :: Int
root = 1

-- | Extract a given sub-'Tree'
tree :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Tree
tree t i = Node (tree t <$> children t i)

-- |
-- @
-- toTree . fromTree = id
-- @
toTree :: (Dictionary t, Elem t ~ Bool) => t -> Tree
toTree t = tree t root

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Int
parent t i = select1 t (rank0 t i)

-- | indices of all of the children of a node
children :: (Dictionary t, Elem t ~ Bool) => t -> Int -> [Int]
children t i = [select0 t r + 1..select0 t (r + 1) - 1]
  where r = rank1 t i

-- | Next sibling, if any
next :: (Dictionary t, Elem t ~ Bool) => t -> Int -> Maybe Int
next t i
  | t ! (i + 1) = Just (i + 1)
  | otherwise = Nothing
