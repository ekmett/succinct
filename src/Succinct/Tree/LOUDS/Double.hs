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
import Succinct.Dictionary.Class
import Succinct.Tree.LOUDS

-- |
-- @Pos i j@ stores @i@ in @1..2n@, the position of a 1 in the LOUDS structure along with
-- @j = rank0 t i@.
--
-- All combinators in this module preserve these invariants.
data Pos = Pos {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving Show

-- | The 'root' of our succinct tree.
root :: Pos
root = Pos 1 0

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: (Dictionary t, Elem t ~ Bool) => t -> Pos -> Pos
parent t (Pos _ j) = Pos i' (i' - j)
  where i' = select1 t j

-- | positions of all of the children of a node
children :: (Dictionary t, Elem t ~ Bool) => t -> Pos -> [Pos]
children t (Pos i _) = [ Pos i' j' | i' <- [select0 t j' + 1..select0 t (j' + 1) - 1] ]
  where j' = rank1 t i

-- | next sibling, if any
next :: (Dictionary t, Elem t ~ Bool) => t -> Pos -> Maybe Pos
next t (Pos i j)
  | i' <- i + 1, t ! i' = Just $ Pos i' j
  | otherwise           = Nothing

-- | Extract a given sub-'Tree'
tree :: (Dictionary t, Elem t ~ Bool) => t -> Pos -> Tree
tree t i = Node (tree t <$> children t i)

-- |
-- @
-- toTree . fromTree = id
-- @
toTree :: (Dictionary t, Elem t ~ Bool) => t -> Tree
toTree t = tree t root
