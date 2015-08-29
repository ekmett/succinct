{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE PatternSynonyms #-}
#endif

module Succinct.Sequence (
  -- $intro
  WaveletTree(..),
  Encoding(..),
  buildOptimizedAlphabeticalSearchTree,
  huTucker,
  validHuTuckerTree,
  ) where

import Control.Applicative
import Control.Monad
import Data.Profunctor
import Data.Bifunctor
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Ord
import Data.List
import Data.Function

import Succinct.Tree.Types
import Succinct.Dictionary.Builder
import Succinct.Internal.Building
import Succinct.Dictionary.Class
import Succinct.Dictionary.Rank9

import Data.Bitraversable
import qualified Data.PriorityQueue.FingerTree as PQ
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Maybe
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe
import Data.Monoid

import Debug.Trace

-- $setup
-- >>> :set -XFlexibleContexts

-- The members of the alphabet need an encoding
newtype Encoding a = Encoding { runEncoding :: Labelled () a } deriving Show

directionToBool :: Direction -> Bool
#if MIN_VERSION_base(4,8,0)
-- We have pattern synonyms
newtype Direction = Direction Bool
instance Show Direction where
  show (Direction False) = show "L"
  show (Direction True) = show "R"
pattern L = Direction False
pattern R = Direction True
directionToBool (Direction x) = x
#else
data Direction = L | R deriving Show
directionToBool L = False
directionToBool R = True
#endif
-- endif MIN_VERSION_base(4,8,0)

{-# INLINE directionToBool #-}

-- | O(nlog(n)). Given elements in a dictionary and their probability of occuring,
-- produce a huffman encoding.
huffmanEncoding :: (Num n, Ord n, Functor m, Monad m) => [(a, n)] -> m (Encoding a)
huffmanEncoding input =
  let initial = PQ.fromList $ fmap (\(a, b) -> (b, LabelledTip a)) input
  in Encoding <$> huffmanHeapToTree initial

huffmanHeapToTree :: (Ord n, Num n, Monad m) => PQ.PQueue n (Labelled () a) -> m (Labelled () a)
huffmanHeapToTree pq = case PQ.minViewWithKey pq of
  Just ((k, v), rest) -> case PQ.minViewWithKey rest of
    Just ((k2, v2), rest2) -> huffmanHeapToTree $ PQ.insert (k+k2) (LabelledBin () v v2) rest2
    Nothing -> return v
  Nothing -> fail "huffmanEncoding: No elements received"

buildHistogram :: (Ord a, Num n) => Builder a (M.Map a n)
buildHistogram = Builder $ Building stop step start
  where
    stop = pure
    step x c = pure $ M.insertWith (+) c 1 x
    start = pure M.empty

buildHuffmanEncoding :: forall a. (Ord a) => Builder a (Encoding a)
buildHuffmanEncoding = fmap (fromJust . huffmanEncoding . M.toList) (buildHistogram :: Builder a (M.Map a Int))

-- | O(n^2). Given a list of characters of an alphabet with a
-- frequency, produce an encoding that respects the order provided.

-- TODO: this is an implementation of Knuth's O(n^2) dynamic
-- programming solution. Hu-Tucker can solve this in O(nlogn) in
-- general.
buildOptimalAlphabeticSearchTree :: [(a, n)] -> Builder a (Encoding a)
buildOptimalAlphabeticSearchTree = undefined

diagonalIndex n i j | trace ("(i, j) = " <> show (i,j)) False = undefined
diagonalIndex n i j = n * i - i * (i + 1) `div` 2 + j

-- data KnuthBuilder s a = KnuthBuilder { weight :: V.STVector s a
--                                      , root :: V.STVector s a
--                                      , path :: V.STVector s a
--                                      }

foldableLength :: F.Foldable f => f a -> Int
#if MIN_VERSION_base(4,8,0)
foldableLength = F.length
#else
foldableLength = length . F.toList
#endif

-- --buildOptimalSearchTree :: (Functor f, F.Foldable f, n ~ Int) => f (a, n) -> Labelled () a
-- buildOptimalSearchTree input = runST $ do
--   let n = foldableLength input
--       space = (n+1) * (n+2) `div` 2
--       index = diagonalIndex (n+1)
--       get v (i, j) = if j < i then error (show (i, j)) else MV.read v (index i j)
--       -- no checks
--       grab v (i, j) = MV.read v (index i j)

--   weight <- MV.new space
--   root <- MV.replicate space (-1)
--   path <- MV.new space
--   forM_ (zip [1..n] $ F.toList $ fmap snd input) $ \(k, p) -> do
--     let i = index k k
--     MV.write weight i p
--     MV.write root i k
--     MV.write path i p
--     MV.write path (index k (k-1)) 0
--   MV.write path (index (n+1) n) 0
--   forM_ [1..n-1] $ \diagonal -> do
--     forM_ [1..n-diagonal] $ \j -> do
--       let k = j + diagonal
--       MV.write weight (index j k) =<< ((+) <$> (weight `get` (j, k-1)) <*> (weight `get` (k, k)))
--       root1 <- root `get` (j, k-1)
--       root2 <- root `get` (j+1, k)
--       (p, _) <- F.minimumBy (comparing snd) <$> (
--         forM [root1..root2] $ \p -> do
--            a <- path `grab` (j, p-1)
--            b <- path `grab` (p+1, k)
--            return (p, a + b))
--       unsafeIOToST $ putStrLn $ "p: " <> show p
--       unsafeIOToST $ putStrLn $ "root" <> show (j,k) <> ": " <> show p
--       MV.write root (index j k) p
--       MV.write path (index j k) =<< liftA3 (\a b c -> a + b + c) (path `grab` (j, p-1)) (path `grab` (p+1, k)) (weight `get` (j, k))

--   let go (i, j) | i > j = error $ "creating tree found i > j: " <> show (i, j)
--       go (i, j) | i == j = pure $ LabelledTip $ i-1
--       go (i, j) = do
--         split <- root `grab` (i, j)
--         LabelledBin (split - 1) <$> go (i, split-1) <*> go (split+1, j)

--   let depth _ (i, j) | i > j = error $ "creating tree found i > j: " <> show (i, j)
--       depth !d (i, j) | i == j = pure $ [(d, i-1)]
--       depth !d (i, j) = do
--         split <- root `grab` (i, j)
--         lefties <- depth (d+1) (i, split-1)
--         righties <- depth (d+1) (split+1, j)
--         return $ [(d, split - 1)] ++ lefties ++ righties

--   depth 0 (1, n)

data Attraction = PreviousPrevious | Previous | Self | Next | NextNext deriving (Show, Eq, Ord)
data Elem heap b a = Elem (Either a (heap b)) [Attraction]
                   | LeftBoundary
                   | RightBoundary
                   deriving (Eq)
instance (Show a, Show b, F.Foldable heap) => Show (Elem heap b a) where
  show (Elem (Left x) a) = "Elem " <> show x <> " " <> show a
  show (Elem (Right h) a) = "Elem " <> show (F.toList h) <> " " <> show a
  show LeftBoundary = "LeftBoundary"
  show RightBoundary = "RightBoundary"

attraction (Elem _ a) = a
attraction LeftBoundary = [Previous]
attraction RightBoundary = [Next]

isBoundary LeftBoundary = True
isBoundary RightBoundary = True
isBoundary _ = False

decideAttraction xs = map f $ filter ok $ tails $ Nothing : fmap Just xs ++ [Nothing]
  where f (Nothing:_:_:_) = Next
        f (_:_:Nothing:_) = Previous
        f (Just a:Just _:Just c:_) = case compare a c of
          LT -> Previous
          _ -> Next
        ok (_:_:_:_) = True
        ok _ = False

--huTucker ::
huTucker = constructTree . breadthFirstSearch . buildOptimizedAlphabeticalSearchTree

-- return tree in increasing depth
breadthFirstSearch :: Labelled () a -> [(a, Int)]
breadthFirstSearch t = go $ S.singleton (t, 0)
  where go to_visit = case S.viewl to_visit of
          S.EmptyL -> []
          (LabelledTip a, !l) S.:< rest -> (a, l) : go rest
          (LabelledBin _ left right, !l) S.:< rest ->
            go (rest S.|> (left, l+1) S.|> (right, l+1))

pair :: [a] -> [(a, a)]
pair [] = []
pair [x] = error "pair: odd man out"
pair (a:b:rest) = (a, b) : pair rest

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN n f = foldl (.) id $ replicate n f

constructTree :: [((Int, a), Int)] -> Labelled () a
constructTree = snd . pairUp . snd . foldl1 (\(old_level, acc) (new_level, new) -> (new_level, merge new $ iterateN (old_level - new_level) bin acc)) . map (\l -> (snd $ head l, map fst l)) . map (sortBy (comparing (fst.fst))) . reverse . groupBy ((==) `on` snd) . map (\((index, value), height) -> ((index, LabelledTip value), height))
  where
    bin x = map (\((i,a), (_,b)) -> (i, LabelledBin () a b)) . pair $ x

    pairUp [] = error "nothing to pair"
    pairUp [x] = x
    pairUp xs = pairUp (bin xs)

    merge a b = sortBy (comparing fst) $ a <> b

codewords :: Labelled () a -> [(a, [Bool])]
codewords t = fmap (\(a, code) -> (a, reverse code)) $ go $ S.singleton (t, [])
  where go to_visit = case S.viewl to_visit of
          S.EmptyL -> []
          (LabelledTip a, code) S.:< rest -> (a, code) : go rest
          (LabelledBin _ left right, code) S.:< rest ->
            go (rest S.|> (left, False:code) S.|> (right, True:code))

buildOptimizedAlphabeticalSearchTree :: forall a n. (Show a, Eq a, Show n, Ord n, Num n, Bounded n) => [(a, n)] -> Labelled () a
buildOptimizedAlphabeticalSearchTree [] = error "Cannot build with empty list of elements"
buildOptimizedAlphabeticalSearchTree input = go (repeat LeftBoundary) $ (<> repeat RightBoundary) $ fmap (\((a, freq), attract) -> Elem (Left (freq, LabelledTip a)) [attract]) $ zip input $ decideAttraction $ fmap snd $ input
  where
    go past [] = error $ "Internal error: empty future. Past: " <> show (takeWhile (not . isBoundary) past)
    go past (RightBoundary:_) = error $ "Internal error: no current. Past: " <> show (takeWhile (not . isBoundary) past)
    go (LeftBoundary:_) (Elem (Left (_, x)) _:RightBoundary:_) = x
    go (LeftBoundary:_) (Elem (Right h) _:RightBoundary:_) = fromJust $ huffmanHeapToTree h
    -- If the person I like likes me back, then we deal with it now.
    go past@(p:past1@(p2:ps)) (x:future@(next:future1@(next2:xs))) =
      case head $ attraction x of
        PreviousPrevious | NextNext `elem` attraction p2 ->
          combine ps (merge (contents p) (fix (contents p2) (contents x))) future
        Previous | Next `elem` attraction p ->
          combine past1 (fix (contents p) (contents x)) future
        Self | Elem (Right heap) _ <- x ->
          let heap' = case PQ.minViewWithKey heap of
                Just ((k, v), rest) -> case PQ.minViewWithKey rest of
                  Just ((k2, v2), rest2) -> PQ.insert (k+k2) (LabelledBin () v v2) rest2
                  Nothing -> error "You shouldn't self attract if you only have one element in the heap"
                Nothing -> error "heap cannot be empty"
          in adjust past (Right heap') future
        Next | Previous `elem` attraction next ->
          combine past (fix (contents x) (contents next)) future1
        NextNext | PreviousPrevious `elem` attraction next2 ->
          let c = merge (fix (contents x) (contents next2)) (contents next)
          in combine past c xs

        -- She loves me not
        _ -> go (x:past) future

    value (Elem (Left (freq, _)) _) = freq
    value (Elem (Right h) _) = heapValue h
    value RightBoundary = maxBound
    value LeftBoundary = maxBound

    contents (Elem x _) = x

    -- Guaranteed that heap is non-empty
    heapValue h = case PQ.minViewWithKey h of
      Just ((k, v), rest) -> k

    secondSmallestElement h = fst <$> (PQ.minViewWithKey =<< snd <$> PQ.minView h)

    combine past@(p:past2) x future@(f:future2) =
      case (isBlocked p, isBlocked f) of
        (False, False) -> adjust past2 (merge (contents p) $ merge x (contents f)) future2
        (False, True) -> adjust past2 (merge (contents p) x) future
        (True, False) -> adjust past (merge x (contents f)) future2
        (True, True) -> adjust past x future

    adjust past x future =
      let e = calculate past x future
          (a:past') = fixPast past (e:future)
          future' = e : fixFuture (e:past) future
      in go past' (a:future')

    fixPast rest@(LeftBoundary:_) _ = rest
    fixPast (Elem heap _:rest@(LeftBoundary:_)) future =
      calculate rest heap future : rest
    fixPast (e1@(Elem heap1 _):rest@(Elem heap2 _:rest2)) future =
      calculate rest heap1 future : calculate rest2 heap2 (e1 : future) : rest2

    fixFuture _ rest@(RightBoundary:_) = rest
    fixFuture past (Elem heap _:rest@(RightBoundary:_)) = calculate past heap rest : rest
    fixFuture past (e1@(Elem heap1 _):rest@(Elem heap2 _:rest2)) = calculate past heap1 rest : calculate (e1 : past) heap2 rest2 : rest2

    ff (Left x) = "Left " <> show x
    ff (Right x) = "Right " <> show (F.toList x)

    calculate (p:p2:_) heap (f:f2:_) =
      Elem heap $
      if can_skip
      then best $ [(value p, Previous), (value f, Next)] ++
           (if not $ isBlocked p then [(value p2, PreviousPrevious)] else []) ++
           (if not $ isBlocked f then [(value f2, NextNext)] else [])
      else case secondSmallestValue of
        Just (k, _) -> best [(value p, Previous), (k, Self), (value f, Next)]
        Nothing -> best [(value p, Previous), (value f, Next)]
      where secondSmallestValue = case heap of
              Left _ -> Nothing
              Right h -> secondSmallestElement h
            can_skip = case heap of
              Left _ -> True
              Right _ -> False
            best = map snd . head . groupBy ((==) `on` fst) . sortBy (comparing fst)

    fix a = Right . fix' a
    fix' (Left (k1, v1)) (Left (k2, v2)) = PQ.singleton (k1 + k2) $ LabelledBin () v1 v2
    fix' (Left (k1, v1)) (Right h) = case PQ.minViewWithKey h of
      Just ((k2, v2), rest) -> PQ.insert (k1 + k2) (LabelledBin () v1 v2) rest
    fix' (Right h) (Left (k2, v2)) = case PQ.minViewWithKey h of
      Just ((k1, v1), rest) -> PQ.insert (k1 + k2) (LabelledBin () v1 v2) rest
    fix' (Right h1) (Right h2) = case PQ.minViewWithKey h1 of
      Just ((k1, v1), rest) -> case PQ.minViewWithKey h2 of
        Just ((k2, v2), rest2) -> PQ.insert (k1 + k2) (LabelledBin () v1 v2) $ rest <> rest2

    merge a b = Right $ f a <> f b
      where f = either (uncurry PQ.singleton) id

    isBlocked (Elem (Right _) _) = False
    isBlocked _ = True

validHuTuckerTree :: (Eq a, Ord a) => Labelled () a -> Bool
validHuTuckerTree t = let nodes = inOrderTraversal t
                      in nodes == sort nodes

inOrderTraversal :: Labelled () a -> [a]
inOrderTraversal (LabelledTip a) = [a]
inOrderTraversal (LabelledBin () l r) = inOrderTraversal l ++ inOrderTraversal r

data WaveletTree f a = WaveletTree { bits :: Labelled f a
                                   , alphabet :: a -> Int -> Direction -- ^ For a given level, is the element to the right or to the left?
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
            LabelledBin x left right ->
              case finda level of
                L -> go (level+1) left (rank0 x i)
                R -> go (level+1) right (rank1 x i)

  select a (WaveletTree t0 find) i0 = findPath 0 t0 i0
    where finda = find a
          findPath level t = case t of
            LabelledTip _ -> id
            LabelledBin x left right ->
              case finda level of
                L -> (select0 x) . findPath (level+1) left
                R -> (select1 x) . findPath (level+1) right

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

buildWithEncoding :: forall a f. Buildable Bool f => Encoding a -> (a -> Int -> Direction) -> Builder a (Labelled f a)
buildWithEncoding (Encoding e) f = Builder $ case builder :: Builder Bool f of
  Builder (Building stop' step' start') ->
    Building stop step start
    where
      start = bifor e (const start') pure
      stop x = bifor x stop' pure
      step x0 c0 = walk c0 0 x0
        where walk _ _ a@(LabelledTip{}) = pure a
              walk c !l (LabelledBin v a b) =
                let dir = f c l
                    q = LabelledBin <$> step' v (directionToBool dir)
                in case dir of
                    L -> q <*> walk c (l+1) a <*> pure b
                    R -> q <*> pure a <*> walk c (l+1) b

decidingFunction :: Eq a => Encoding a -> (a -> Int -> Direction)
decidingFunction (Encoding enc0) = \c -> let Just t = fmap reverse $ go enc0 c [] in (t !!)
  where go (LabelledTip c') c visited = do
          guard $ c == c'
          return $ visited
        go (LabelledBin _ l _) c visited | Just x <- go l c (L:visited ) = Just x
        go (LabelledBin _ _ r) c visited | Just x <- go r c (R:visited ) = Just x
        go _ _ _ = Nothing


instance (Ord a, Buildable Bool f) => Buildable a (WaveletTree f a) where
  builder = runNonStreaming $ do
    enc <- NonStreaming $ buildHuffmanEncoding
    let fun = decidingFunction enc
    NonStreaming $ WaveletTree <$> buildWithEncoding enc fun <*> pure fun
