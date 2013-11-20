{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Succinct.Internal.Builder
  ( Builder(..)
  , Buildable(..)
  , build, buildWith
  , vector
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Foldable as F
import Data.Profunctor
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Primitive as P
import Data.Vector.Internal.Check as Ck

#define INTERNAL_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Internal

data Builder s a b where
  Builder :: (x -> ST s b) -> (x -> a -> ST s x) -> ST s x -> Builder s a b

instance Profunctor (Builder s) where
  dimap f g (Builder k h z) = Builder (fmap g . k) (\x a -> h x (f a)) z
  {-# INLINE dimap #-}

instance Functor (Builder s a) where
  fmap f (Builder k h z) = Builder (fmap f . k) h z
  {-# INLINE fmap #-}

instance Applicative (Builder s a) where
  pure b = Builder (\() -> return b) (\() _ -> return ()) (return ())
  {-# INLINE pure #-}
  Builder kf hf zf <*> Builder ka ha za = Builder
    (\(Pair xf xa) -> kf xf <*> ka xa)
    (\(Pair xf xa) a -> Pair <$> hf xf a <*> ha xa a)
    (Pair <$> zf <*> za)
  {-# INLINE (<*>) #-}

data Pair a b = Pair !a !b

build :: (Foldable f, Buildable a b) => f a -> b
build = buildWith builder
{-# INLINE build #-}

buildWith :: Foldable f => (forall s. Builder s a b) -> f a -> b
buildWith m as = runST $ case m of
  Builder k h z -> do
    b <- z
    k =<< F.foldlM h b as
{-# INLINE buildWith #-}

class Buildable a b | b -> a where
  builder :: Builder s a b

instance Buildable a [a] where
  builder = Builder (\k -> return $ k []) (\f a -> return $ f . (a:)) (return id)
  {-# INLINE builder #-}

instance U.Unbox a => Buildable a (U.Vector a) where
  builder = vector
  {-# INLINE builder #-}

instance P.Prim a => Buildable a (P.Vector a) where
  builder = vector
  {-# INLINE builder #-}

instance Buildable a (V.Vector a) where
  builder = vector
  {-# INLINE builder #-}

data V a = V {-# UNPACK #-} !Int !a

vector :: G.Vector v a => Builder s a (v a)
vector = Builder stop step start where
  start = V 0 `liftM` GM.unsafeNew 0
  step (V i v) x = do
    v' <- unsafeAppend1 v i x
    return $! V (i + 1) v'
  stop (V n v)
    = G.unsafeFreeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (GM.length v)
    $ GM.unsafeSlice 0 n v
{-# INLINE vector #-}

unsafeAppend1 :: GM.MVector v a => v s a -> Int -> a -> ST s (v s a)
unsafeAppend1 v i x
  | i < GM.length v = do
    GM.unsafeWrite v i x
    return v
  | otherwise = do
    v' <- enlarge v
    INTERNAL_CHECK(checkIndex) "unsafeAppend1" i (GM.length v')
      $ GM.unsafeWrite v' i x
    return v'
{-# INLINE unsafeAppend1 #-}

-- | Grow a vector logarithmically
enlarge :: GM.MVector v a => v s a -> ST s (v s a)
enlarge v = GM.unsafeGrow v (max (GM.length v) 1)
{-# INLINE enlarge #-}
