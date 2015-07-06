{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Succinct.Internal.Building
  ( Building(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Profunctor

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
