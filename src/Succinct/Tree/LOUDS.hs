{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Delpratt, Rahman and Raman's double numbering scheme for LOUDS
module Succinct.Tree.LOUDS
  (
  -- * Basic rose tree
    Rose(..)
  -- * Conversion
  , rose
  , toRose
  , fromRose
  , louds
  -- * LOUDS zipper
  , Zipper(..)
  -- * Operations
  , root
  , index
  , top
  , parent
  , children
  , next
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Proxy
import Data.Word
import Data.Traversable
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9
import Succinct.Tree.Types (Rose(..))
import Succinct.Internal.Bit (PackedBits(Packed))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

-- | Jacobson's 1-based LOUDS
--
-- Visit every node in level order. Represent each node with @n@ children by @(1^n)0@.
--
-- We add an extra @10@ \"superroot\" to the top of the tree to avoid corner cases.
louds :: Rose -> [Bool]
louds xs = True: False: go 0 where
  go n = case level n xs [] of
    [] -> []
    ys -> ys ++ go (n+1)
{-# INLINE louds #-}

-- | Convert a finite 'Rose' to 'Rank9'
fromRose :: (Bitwise [Bool] U.Vector)
         => Rose -> Rank9 (Packed U.Vector)
fromRose = fromRose' (Proxy :: Proxy U.Vector)
{-# INLINE fromRose #-}

-- | Convert a finite 'Rose' to 'Rank9'
fromRose' :: ( Bitwise [Bool] v, PackedBits v
             , G.Vector (Packed v) Word64, G.Vector v Word64)
          => Proxy v -> Rose -> Rank9 (Packed v)
fromRose' p = rank9 p . louds
{-# INLINE fromRose' #-}

level :: Int -> Rose -> [Bool] -> [Bool]
level 0 (Rose cs) xs = replicate (length cs) True ++ (False:xs)
level n (Rose cs) xs = Prelude.foldr (level (n - 1)) xs cs

-- |
-- @Zipper i j t@ stores @i@ in @1..2n@, the position of a 1 in the LOUDS structure @t@ along with
-- @j = rank0 t i@.
--
-- All combinators in this module preserve these invariants.
data Zipper t = Zipper {-# UNPACK #-} !Int {-# UNPACK #-} !Int t
  deriving (Eq,Show)

instance Access a t => Access a (Zipper t) where
  size (Zipper _ _ t) = size t
  {-# INLINE size #-}
  (!) (Zipper _ _ t) i = t ! i
  {-# INLINE (!) #-}

instance Dictionary a t => Dictionary a (Zipper t) where
  rank a (Zipper _ _ t) i = rank a t i
  {-# INLINE rank #-}
  select a (Zipper _ _ t) i = select a t i
  {-# INLINE select #-}

instance Select0 t => Select0 (Zipper t) where
  select0 (Zipper _ _ t) i = select0 t i
  {-# INLINE select0 #-}

instance Select1 t => Select1 (Zipper t) where
  select1 (Zipper _ _ t) i = select1 t i
  {-# INLINE select1 #-}

instance Ranked t => Ranked (Zipper t) where
  rank0 (Zipper _ _ t) i = rank0 t i
  {-# INLINE rank0 #-}
  rank1 (Zipper _ _ t) i = rank1 t i
  {-# INLINE rank1 #-}

instance Functor Zipper where
  fmap f (Zipper i j a) = Zipper i j (f a)
  {-# INLINE fmap #-}

instance Foldable Zipper where
  foldMap f (Zipper _ _ a) = f a
  {-# INLINE foldMap #-}

instance Traversable Zipper where
  traverse f (Zipper i j a) = Zipper i j <$> f a
  {-# INLINE traverse #-}

instance Comonad Zipper where
  extract (Zipper _ _ a) = a
  {-# INLINE extract #-}
  duplicate w@(Zipper i j _) = Zipper i j w
  {-# INLINE duplicate #-}

-- | The 'root' of our succinct tree.
root :: t -> Zipper t
root = Zipper 1 0
{-# INLINE root #-}

-- | The index of this node in level order, starting at 1 for the root.
index :: Zipper t -> Int
index (Zipper i j _) = i - j
{-# INLINE index #-}

-- | Is this node the 'root'?
top :: Zipper t -> Bool
top (Zipper i _ _) = i == 1
{-# INLINE top #-}

-- | The parent of any node @i /= root@, obtained by a legal sequence of operations.
parent :: Select1 t => Zipper t -> Zipper t
parent (Zipper _ j t) = Zipper i' (i' - j) t
  where i' = select1 t j
{-# INLINE parent #-}

-- | positions of all of the children of a node
children :: Select0 t => Zipper t -> [Zipper t]
children (Zipper i j t) = do
  let j' = i - j
  i' <- [select0 t j' + 1..select0 t (j' + 1) - 1]
  return $ Zipper i' j' t
{-# INLINE children #-}

-- | next sibling, if any
next :: Access Bool t => Zipper t -> Maybe (Zipper t)
next (Zipper i j t)
  | i' <- i + 1, t ! i' = Just $ Zipper i' j t
  | otherwise           = Nothing
{-# INLINE next #-}

-- | Extract a given sub-tree
rose :: Select0 t => Zipper t -> Rose
rose i = Rose (rose <$> children i)
{-# INLINE rose #-}

-- |
-- @
-- toRose . fromRose = id
-- @
toRose :: Select0 t => t -> Rose
toRose = rose . root
{-# INLINE toRose #-}
