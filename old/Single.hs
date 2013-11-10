{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Succinct.Tree.LOUDS.Zipper.Single
  (
  -- * Single indexing
    Zipper(..)
  , toTree
  , root
  , top
  , parent
  , children
  , next
  -- * LOUDS tree representaton
  , module Succinct.Tree.LOUDS.Encoding
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Traversable
import Succinct.Dictionary.Class
import Succinct.Tree.LOUDS

-- | single indexed LOUDS tree-zipper a la Jacobson
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

-- | The 'root' of our succinct tree.
root :: t -> Zipper t
root = Zipper 1

-- | Is this node the 'root'?
top :: Zipper t -> Bool
top (Zipper i _) = i == 1

-- | Extract a given sub-'Tree'
tree :: Ranked t => Zipper t -> Tree
tree z = Node (tree <$> children z)

-- |
-- @
-- toTree . fromTree = id
-- @
toTree :: Ranked t => t -> Tree
toTree = tree . root

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: Ranked t => Zipper t -> Zipper t
parent (Zipper i t) = Zipper (select1 t (rank0 t i)) t

-- | indices of all of the children of a node
children :: Ranked t => Zipper t -> [Zipper t]
children (Zipper i t) = [ Zipper i' t | i' <- [select0 t j + 1..select0 t (j + 1) - 1] ]
  where j = rank1 t i

-- | Next sibling, if any
next :: Access Bool t => Zipper t -> Maybe (Zipper t)
next (Zipper i t)
  | t ! (i + 1) = Just $ Zipper (i + 1) t
  | otherwise = Nothing
