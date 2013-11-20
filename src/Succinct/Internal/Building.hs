{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Succinct.Internal.Building
  ( Building(..)
  ) where

import Control.Applicative
import Control.Monad.ST
import Data.Profunctor

data Building s a b where
  Building :: (x -> ST s b) -> (x -> a -> ST s x) -> ST s x -> Building s a b

instance Profunctor (Building s) where
  dimap f g (Building k h z) = Building (fmap g . k) (\x a -> h x (f a)) z
  {-# INLINE dimap #-}

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

instance Functor (Building s a) where
  fmap f (Building k h z) = Building (fmap f . k) h z
  {-# INLINE fmap #-}

instance Applicative (Building s a) where
  pure b = Building (\() -> return b) (\() _ -> return ()) (return ())
  {-# INLINE pure #-}
  Building kf hf zf <*> Building ka ha za = Building
    (\(Pair xf xa) -> kf xf <*> ka xa)
    (\(Pair xf xa) a -> Pair <$> hf xf a <*> ha xa a)
    (Pair <$> zf <*> za)
  {-# INLINE (<*>) #-}

data Pair a b = Pair !a !b
