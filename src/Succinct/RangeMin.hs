-- TODO: add a Poppy-style 2-level hierarcy
module Succinct.RangeMin
  ( RangeMin(..)
  , fromBits
  ) where

import Succinct.Dictionary.Class
import Succinct.Internal.Bit
import Succinct.Internal.Level
import Data.Vector as V
import Data.Vector.Unboxed as U
import Data.Vector.Primitive as P
import Data.Word

-- delta rangemin tree
data RangeMin = RangeMin
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(P.Vector Word64)
  {-# UNPACK #-} !(V.Vector Level)

instance Bitwise RangeMin where
  bitwise (RangeMin n bs _) = V_Bit n bs

fromBits :: U.Vector Bit -> RangeMin
fromBits (V_Bit n bs) = RangeMin n bs (levels bs)
