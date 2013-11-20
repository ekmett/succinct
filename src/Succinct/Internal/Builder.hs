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
  , Building(..)
  , Buildable(..)
  , build
  , buildWith
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

newtype Builder a b = Builder (forall s. Building s a b)

data Building s a b where
  Building :: (x -> ST s b) -> (x -> a -> ST s x) -> ST s x -> Building s a b

instance Profunctor (Building s) where
  dimap f g (Building k h z) = Building (fmap g . k) (\x a -> h x (f a)) z
  {-# INLINE dimap #-}

instance Profunctor Builder where
  dimap f g (Builder k) = Builder (dimap f g k)

instance Choice (Building s) where
  left' (Building k h z) = Building (_Left k) step (Left <$> z) where
    step (Left x) (Left y) = Left <$> h x y
    step (Right c) _ = pure $ Right c
    step _ (Right c) = pure $ Right c
    _Left f (Left a) = Left <$> f a
    _Left _ (Right b) = pure $ Right b

  right' (Building k h z) = Building (_Right k) step (Right <$> z) where
    step (Right x) (Right y) = Right <$> h x y
    step (Left c) _ = pure $ Left c
    step _ (Left c) = pure $ Left c
    _Right _ (Left b) = pure $ Left b
    _Right f (Right a) = Right <$> f a

instance Choice Builder where
  left' (Builder k) = Builder (left' k)
  right' (Builder k) = Builder (right' k)

instance Functor (Building s a) where
  fmap f (Building k h z) = Building (fmap f . k) h z
  {-# INLINE fmap #-}

instance Functor (Builder a) where
  fmap f (Builder k) = Builder (fmap f k)

instance Applicative (Building s a) where
  pure b = Building (\() -> return b) (\() _ -> return ()) (return ())
  {-# INLINE pure #-}
  Building kf hf zf <*> Building ka ha za = Building
    (\(Pair xf xa) -> kf xf <*> ka xa)
    (\(Pair xf xa) a -> Pair <$> hf xf a <*> ha xa a)
    (Pair <$> zf <*> za)
  {-# INLINE (<*>) #-}

instance Applicative (Builder a) where
  pure a = Builder (pure a)
  Builder mf <*> Builder ma = Builder (mf <*> ma)

data Pair a b = Pair !a !b

build :: (Foldable f, Buildable a b) => f a -> b
build = buildWith builder
{-# INLINE build #-}

buildWith :: Foldable f => Builder a b -> f a -> b
buildWith (Builder m) as = runST $ case m of
  Building k h z -> do
    b <- z
    k =<< F.foldlM h b as
{-# INLINE buildWith #-}

class Buildable a b | b -> a where
  builder :: Builder a b

instance Buildable a [a] where
  builder = Builder $ Building (\k -> return $ k []) (\f a -> return $ f . (a:)) (return id)
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

vector :: G.Vector v a => Builder a (v a)
vector = Builder building where
  building = Building stop step start where
   start = V 0 `liftM` GM.unsafeNew 0
   step (V i v) x = do
     v' <- unsafeAppend1 v i x
     return $! V (i + 1) v'
   stop (V n v)
     = G.unsafeFreeze
     $ INTERNAL_CHECK(checkSlice) "Builder.vector" 0 n (GM.length v)
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
