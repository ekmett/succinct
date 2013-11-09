{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Delpratt, Rahman and Raman's double numbering scheme for LOUDS
module Succinct.Tree.LOUDS.Double
  (
  -- * Delpratt, Rahman and Raman's double numbering
    Zipper(..)
  , root
  , index
  , top
  , parent
  , children
  , next
  , tree
  , toTree
  -- * LOUDS tree representation
  , module Succinct.Tree.LOUDS
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Traversable
import Succinct.Dictionary.Class
import Succinct.Tree.LOUDS

-- |
-- @Zipper i j t@ stores @i@ in @1..2n@, the position of a 1 in the LOUDS structure @t@ along with
-- @j = rank0 t i@.
--
-- All combinators in this module preserve these invariants.
data Zipper t = Zipper {-# UNPACK #-} !Int {-# UNPACK #-} !Int t
  deriving (Eq,Show)

instance Access a t => Access a (Zipper t) where
  size (Zipper _ _ t) = size t
  (!) (Zipper _ _ t) i = t ! i

instance Dictionary a t => Dictionary a (Zipper t) where
  rank a (Zipper _ _ t) i = rank a t i
  select a (Zipper _ _ t) i = select a t i

instance Select0 t => Select0 (Zipper t) where
  select0 (Zipper _ _ t) i = select0 t i

instance Select1 t => Select1 (Zipper t) where
  select1 (Zipper _ _ t) i = select1 t i

instance Functor Zipper where
  fmap f (Zipper i j a) = Zipper i j (f a)

instance Foldable Zipper where
  foldMap f (Zipper _ _ a) = f a

instance Traversable Zipper where
  traverse f (Zipper i j a) = Zipper i j <$> f a

instance Comonad Zipper where
  extract (Zipper _ _ a) = a
  duplicate w@(Zipper i j _) = Zipper i j w

-- | The 'root' of our succinct tree.
root :: t -> Zipper t
root = Zipper 1 0

-- | The index of this node in level order, starting at 1 for the root.
index :: Zipper t -> Int
index (Zipper i j _) = i - j

-- | Is this node the 'root'?
top :: Zipper t -> Bool
top (Zipper i _ _) = i == 1

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: Select1 t => Zipper t -> Zipper t
parent (Zipper _ j t) = Zipper i' (i' - j) t
  where i' = select1 t j

-- | positions of all of the children of a node
children :: Select0 t => Zipper t -> [Zipper t]
children (Zipper i j t) = [ Zipper i' j' t | i' <- [select0 t j' + 1..select0 t (j' + 1) - 1] ]
  where j' = i - j

-- | next sibling, if any
next :: Access Bool t => Zipper t -> Maybe (Zipper t)
next (Zipper i j t)
  | i' <- i + 1, t ! i' = Just $ Zipper i' j t
  | otherwise           = Nothing

-- | Extract a given sub-'Tree'
tree :: Select0 t => Zipper t -> Tree
tree i = Node (tree <$> children i)

-- |
-- @
-- toTree . fromTree = id
-- @
toTree :: Ranked t => t -> Tree
toTree = tree . root
