module Succinct.Tree.Types
  ( Binary(..)
  , Labelled(..)
  , Rose(..)
  ) where

import Data.Bifunctor

data Binary = Bin Binary Binary | Tip deriving (Eq,Show)
data Labelled b a = LabelledBin b (Labelled b a) (Labelled b a) | LabelledTip a deriving (Eq,Show)

instance Bifunctor Labelled where
  bimap f g (LabelledBin x a b) = LabelledBin (f x) (bimap f g a) (bimap f g b)
  bimap _ g (LabelledTip x) = LabelledTip $ g x

newtype Rose = Rose [Rose] deriving (Eq,Show)
