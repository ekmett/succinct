{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

-- | Delpratt, Rahman and Raman's double numbering scheme for LOUDS
module Succinct.Tree.LOUDS.Double
  (
  -- * Delpratt, Rahman and Raman's double numbering
    Pos(..)
  , root
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
-- @Pos i j t@ stores @i@ in @1..2n@, the position of a 1 in the LOUDS structure @t@ along with
-- @j = rank0 t i@.
--
-- All combinators in this module preserve these invariants.
data Pos t = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int t
  deriving Show

instance Dictionary t => Dictionary (Pos t) where
  type Elem (Pos t) = Elem t
  size (Pos _ _ t) = size t
  rank a (Pos _ _ t) i = rank a t i
  (!) (Pos _ _ t) i = t ! i
  select a (Pos _ _ t) i = select a t i

instance Functor Pos where
  fmap f (Pos i j a) = Pos i j (f a)

instance Foldable Pos where
  foldMap f (Pos _ _ a) = f a

instance Traversable Pos where
  traverse f (Pos i j a) = Pos i j <$> f a

instance Comonad Pos where
  extract (Pos _ _ a) = a
  duplicate w@(Pos i j _) = Pos i j w

-- | The 'root' of our succinct tree.
root :: t -> Pos t
root = Pos 1 0

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: (Dictionary t, Elem t ~ Bool) => Pos t -> Pos t
parent (Pos _ j t) = Pos i' (i' - j) t
  where i' = select1 t j

-- | positions of all of the children of a node
children :: (Dictionary t, Elem t ~ Bool) => Pos t -> [Pos t]
children (Pos i _ t) = [ Pos i' j' t | i' <- [select0 t j' + 1..select0 t (j' + 1) - 1] ]
  where j' = rank1 t i

-- | next sibling, if any
next :: (Dictionary t, Elem t ~ Bool) => Pos t -> Maybe (Pos t)
next (Pos i j t)
  | i' <- i + 1, t ! i' = Just $ Pos i' j t
  | otherwise           = Nothing

-- | Extract a given sub-'Tree'
tree :: (Dictionary t, Elem t ~ Bool) => Pos t -> Tree
tree i = Node (tree <$> children i)

-- |
-- @
-- toTree . fromTree = id
-- @
toTree :: (Dictionary t, Elem t ~ Bool) => t -> Tree
toTree = tree . root
