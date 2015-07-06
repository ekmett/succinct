{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Succinct.Dictionary.Partitioned
  ( Partitioned(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class

-- | A partitioned succinct dictionary only supports 'select0' and 'select1' operations
--
-- This is sufficient to implement LOUDS with double-indexing
data Partitioned t = PE | P0 t t | P1 t t
  deriving (Eq,Ord,Show)

instance Functor Partitioned where
  fmap _ PE = PE
  fmap f (P0 r0 r1) = P0 (f r0) (f r1)
  fmap f (P1 r0 r1) = P1 (f r0) (f r1)

instance Foldable Partitioned where
  foldMap _ PE = mempty
  foldMap f (P0 r0 r1) = f r0 `mappend` f r1
  foldMap f (P1 r0 r1) = f r0 `mappend` f r1

instance Traversable Partitioned where
  traverse _ PE = pure PE
  traverse f (P0 r0 r1) = P0 <$> f r0 <*> f r1
  traverse f (P1 r0 r1) = P1 <$> f r0 <*> f r1

instance Ranked t => Select0 (Partitioned t) where
  select0 PE _ = error "Partitioned.select0: empty"
  select0 (P0 r0 r1) i = select1 r1 (rank_ r0 i) + i
  select0 (P1 r0 r1) i = select1 r1 (rank_ r0 i + 1) + i

instance Ranked t => Select1 (Partitioned t) where
  select1 PE _ = error "Partitioned.select1: empty"
  select1 (P0 r0 r1) i = select1 r0 (rank_ r1 i + 1) + i
  select1 (P1 r0 r1) i = select1 r0 (rank_ r1 i) + i

data BP a b
  = BE !(Maybe Bool) a b
  | BT !Bool a b !Bool

p :: Bool -> a -> a -> Partitioned a
p False = P0
p True = P1

instance Buildable Bool a => Buildable Bool (Partitioned a) where
  builder = Builder $ case builder of
    Builder (Building k h z) -> Building stop step start where
      start = BE Nothing <$> z <*> z

      step (BE Nothing x y) b  = return $ BE (Just b) x y
      step (BE (Just b) x y) c = return $ BT b x y c
      step (BT b x y True) c = do
        y' <- h y (not c)
        return $ BT b x y' c
      step (BT b x y False) c = do
        x' <- h x c
        return $ BT b x' y c

      stop (BE Nothing _ _)  = return PE
      stop (BE (Just b) x y) = p b <$> k x <*> k y
      stop (BT b x y False) = do
        x' <- h x True
        p b <$> k x' <*> k y
      stop (BT b x y True) = do
        y' <- h y True
        p b <$> k x <*> k y'
