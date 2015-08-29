{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2013-15 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Succinct.Tree.Types
  ( Binary(..)
  , Labelled(..)
  , labelledToTree
  , drawLabelled
  , Rose(..)
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Tree
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Binary = Bin Binary Binary | Tip deriving (Eq,Show)
data Labelled b a = LabelledBin b (Labelled b a) (Labelled b a) | LabelledTip a deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

labelledToTree :: Labelled b a -> Tree (Either b a)
labelledToTree (LabelledTip x) = Node (Right x) []
labelledToTree (LabelledBin x l r) = Node (Left x) [labelledToTree l, labelledToTree r]

drawLabelled :: (Show a, Show b) => Labelled b a -> String
drawLabelled = drawTree . fmap show . labelledToTree

instance Bifunctor Labelled where
  bimap f g (LabelledBin x a b) = LabelledBin (f x) (bimap f g a) (bimap f g b)
  bimap _ g (LabelledTip x) = LabelledTip $ g x

instance Bifoldable Labelled where
  bifoldMap = bifoldMapDefault

instance Bitraversable Labelled where
  bitraverse _ g (LabelledTip x) = LabelledTip <$> g x
  bitraverse f g (LabelledBin x a b) = LabelledBin <$> f x <*> bitraverse f g a <*> bitraverse f g b


newtype Rose = Rose [Rose] deriving (Eq,Show)
