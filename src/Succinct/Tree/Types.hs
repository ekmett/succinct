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
  , Rose(..)
  ) where

data Binary = Bin Binary Binary | Tip deriving (Eq,Show)

newtype Rose = Rose [Rose] deriving (Eq,Show)
