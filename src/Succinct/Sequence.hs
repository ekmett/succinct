{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Succinct.Sequence (
  -- $intro
  WaveletTree(..),
  Encoding(..)
  ) where

import Control.Applicative
import Data.Profunctor
import Data.Bits
import qualified Data.Foldable as F

import Succinct.Tree.Types
import Succinct.Dictionary.Builder
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9

-- $setup
-- >>> :set -XFlexibleContexts

-- The members of the alphabet need an encoding
newtype Encoding a = Encoding { runEncoding :: Labelled () a }

-- | Given a list of members of an alphabet in order with a frequency, produce an encoding that respects the order.
--huTucker :: [(a, Int)] -> Encoding a
--huTucker xs = _

data WaveletTree f a = WaveletTree { bits :: Labelled f a
                                   , alphabet :: a -> Int -> Bool -- ^ For a given level, is the element to the right or to the left?
                                   }

instance (Access Bool f, Ranked f) => F.Foldable (WaveletTree f) where
  foldMap f t = F.foldMap f $ map (t !) [0 .. size t - 1]

-- mapBits f (WaveletTree bits alphabet) = WaveletTree (first f bits) alphabet

instance (Access Bool f, Ranked f) => Access a (WaveletTree f a) where
  size (WaveletTree t _) = case t of
    LabelledTip _ -> 0
    LabelledBin x _ _ -> size x
  (!) (WaveletTree t0 _) index0 = go t0 index0
    where go t index = case t of
                        LabelledTip a -> a
                        LabelledBin x left right ->
                          if x ! index
                          then go right (rank1 x index)
                          else go left (rank0 x index)

instance (Access Bool f, Select0 f, Select1 f, Ranked f) => Dictionary a (WaveletTree f a) where
  rank a (WaveletTree t0 find) i0 = go 0 t0 i0
    where finda = find a
          go level t i = case t of
            LabelledTip _ -> i
            LabelledBin x left right -> if finda level 
                                        then go (level+1) right (rank1 x i)
                                        else go (level+1) left (rank0 x i)

  select a (WaveletTree t0 find) i0 = findPath 0 t0 i0
    where finda = find a
          findPath level t = case t of
            LabelledTip _ -> id
            LabelledBin x left right -> if finda level
                                        then (select1 x) . findPath (level+1) right
                                        else (select0 x) . findPath (level+1) left

-- $intro
-- >>> :{
-- let abracadabraFind 'a' = const False
--     abracadabraFind 'b' = odd
--     abracadabraFind 'c' = (> 0)
--     abracadabraFind 'd' = even
--     abracadabraFind 'r' = const True
-- :}
-- 
-- >>> :{
-- let bin = LabelledBin ()
--     tip = LabelledTip
--     abracadabraEncoding =
--       bin (bin (tip 'a') (bin (tip 'b') (tip 'c'))) (bin (tip 'd') (tip 'r'))
-- :}
--
-- >>> :{
-- let (t, f) = (True, False)
--     tree = LabelledBin (build [f, f, t, f, f, f, t, f, f, t, f])
--            -- a & (b & c)
--            (LabelledBin (build [f, t, f, t, f, f, t, f]) (LabelledTip 'a')
--             -- b & c
--             (LabelledBin (build [f, t, f]) (LabelledTip 'b') (LabelledTip 'c')))
--            -- d & r
--            (LabelledBin (build [t, f, t]) (LabelledTip 'd') (LabelledTip 'r'))
--     exampleWaveletTree :: WaveletTree Rank9 Char
--     exampleWaveletTree = WaveletTree tree abracadabraFind
-- :}
--
-- Inefficient foldable 
--
-- >>> F.toList exampleWaveletTree
-- "abracadabra"
-- 
-- >>> [(c, map (select c exampleWaveletTree) [1.. rank c exampleWaveletTree (size exampleWaveletTree)]) | c <- "abcdr"]
-- [('a',[1,4,6,8,11]),('b',[2,9]),('c',[5]),('d',[7]),('r',[3,10])]
--
-- >>> [(c, map (rank c exampleWaveletTree) [0.. size exampleWaveletTree]) | c <- "abcdr"]
-- [('a',[0,1,1,1,2,2,3,3,4,4,4,5]),('b',[0,0,1,1,1,1,1,1,1,2,2,2]),('c',[0,0,0,0,0,1,1,1,1,1,1,1]),('d',[0,0,0,0,0,0,0,1,1,1,1,1]),('r',[0,0,0,1,1,1,1,1,1,1,2,2])]

asListOfNumbers :: Access Bool t => t -> [Int]
asListOfNumbers t = concat [ [x | t ! x] | x <- [0 .. size t - 1] ]

data WTBuild = WTBuild

split :: (a -> Bool) -> Builder a (f a, 

buildWithEncoding :: Buildable Bool f => Encoding a -> (a -> Int -> Bool) -> Builder a (Labelled f a)
buildWithEncoding (Encoding encoding0) f = go encoding0 0
  where go encoding level = case encoding of
          LabelledTip a -> pure $ LabelledTip a
          LabelledBin () left right -> do
            (choices, l, r) <- split (`f` level)
            LabelledBin choices <$> go left (level+1) <*> go rright (level+1)
            --LabelledBin <$> lmap (`f` level) builder <*> go left (level+1) <*> go right (level+1)

instance Buildable Bool f => Buildable a (WaveletTree f a) where
  builder = undefined
