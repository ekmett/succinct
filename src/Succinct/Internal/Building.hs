{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Succinct.Internal.Building
  ( Building(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.Profunctor
import qualified Data.Foldable as F

-- | @foldlM@ as a data structure
data Building m a b where
  Building :: (x -> m b) -> (x -> a -> m x) -> m x -> Building m a b

instance Functor m => Profunctor (Building m) where
  dimap f g (Building k h z) = Building (fmap g . k) (\x a -> h x (f a)) z
  {-# INLINE dimap #-}
  rmap g (Building k h z) = Building (fmap g . k) h z
  {-# INLINE rmap #-}
  lmap f (Building k h z) = Building k (\x a -> h x (f a)) z
  {-# INLINE lmap #-}

instance Applicative m => Choice (Building m) where
  left' (Building k h z) = Building (_Left k) step (Left <$> z) where
    step (Left x) (Left y) = Left <$> h x y
    step (Right c) _ = pure $ Right c
    step _ (Right c) = pure $ Right c

    _Left f (Left a) = Left <$> f a
    _Left _ (Right b) = pure $ Right b
  {-# INLINE left' #-}

  right' (Building k h z) = Building (_Right k) step (Right <$> z) where
    step (Right x) (Right y) = Right <$> h x y
    step (Left c) _ = pure $ Left c
    step _ (Left c) = pure $ Left c

    _Right _ (Left b)  = pure $ Left b
    _Right f (Right a) = Right <$> f a
  {-# INLINE right' #-}

instance Functor m => Functor (Building m a) where
  fmap f (Building k h z) = Building (fmap f . k) h z
  {-# INLINE fmap #-}

instance Applicative m => Applicative (Building m a) where
  pure b = Building (\() -> pure b) (\() _ -> pure ()) (pure ())
  {-# INLINE pure #-}
  Building kf hf zf <*> Building ka ha za = Building
    (\(Pair xf xa) -> kf xf <*> ka xa)
    (\(Pair xf xa) a -> Pair <$> hf xf a <*> ha xa a)
    (Pair <$> zf <*> za)
  {-# INLINE (<*>) #-}

data Pair a b = Pair !a !b

reduce :: (Monad m, F.Foldable f) => f a -> Building m a b -> m b
reduce as (Building k h z) = do
  b <- z
  k =<< F.foldlM h b as

join' :: Monad m => Building m a (Building m a b) -> Building m a b
join' (Building k h z) = Building
                         (\(Pair z as) -> reduce as =<< k z)
                         (\(Pair z as) a -> fmap (`Pair` (a : as)) $ h z a)
                         (fmap (`Pair` []) z)

-- | Monad instance is not streaming; prefer the applicative.
instance Monad m => Monad (Building m a) where
  return = pure
  {-# INLINE return #-}
  Building ka ha za >>= f = join' $ Building (fmap f . ka) ha za
  {-# INLINE (>>=) #-}

split :: (F.Foldable f) =>
         (a -> Bool) -> Building m Bool t -> Building m a b -> Building m a (t, Building m a b, Building m a b)
split pred (Building kt ht zt) (Building kb hb zb) =
  Building () (\(Trips x l r) a -> let b = f a
                                   in if f a
                                      then liftA3 Trips (ht x b) l 
) ()
