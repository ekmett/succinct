module Succinct.Tree.Types
  ( Binary(..)
  , Rose(..)
  ) where

data Binary = Bin Binary Binary | Tip deriving (Eq,Show)

newtype Rose = Rose [Rose] deriving (Eq,Show)
