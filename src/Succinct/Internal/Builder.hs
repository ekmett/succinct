{-# LANGUAGE CPP #-}
module Succinct.Internal.Builder
  ( Builder(..)
  , newBuilder
  , snoc
  , runBuilder
  ) where

import Control.Monad
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable as GM
import Data.Vector.Internal.Check as Ck

#define INTERNAL_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Internal

data Builder v s a = Builder {-# UNPACK #-} !Int !(v s a)

newBuilder :: (PrimMonad m, MVector v a) => m (Builder v (PrimState m) a)
newBuilder = Builder 0 `liftM` unsafeNew 0

snoc :: (PrimMonad m, MVector v a) => Builder v (PrimState m) a -> a -> m (Builder v (PrimState m) a)
snoc (Builder i v) x = do
  v' <- unsafeAppend1 v i x
  return $ Builder (i + 1) v'

runBuilder :: (PrimMonad m, MVector v a) => Builder v (PrimState m) a -> v (PrimState m) a
runBuilder (Builder n v) = INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (GM.length v)
                         $ GM.unsafeSlice 0 n v

unsafeAppend1 :: (PrimMonad m, MVector v a)
        => v (PrimState m) a -> Int -> a -> m (v (PrimState m) a)
    -- NOTE: The case distinction has to be on the outside because
    -- GHC creates a join point for the unsafeWrite even when everything
    -- is inlined. This is bad because with the join point, v isn't getting
    -- unboxed.
unsafeAppend1 v i x
  | i < GM.length v = do
                     GM.unsafeWrite v i x
                     return v
  | otherwise    = do
                     v' <- enlarge v
                     INTERNAL_CHECK(checkIndex) "unsafeAppend1" i (GM.length v')
                       $ GM.unsafeWrite v' i x
                     return v'
{-# INLINE unsafeAppend1 #-}

enlarge_delta :: MVector v a => v s a -> Int
enlarge_delta v = max (GM.length v) 1

-- | Grow a vector logarithmically
enlarge :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
enlarge v = unsafeGrow v (enlarge_delta v)
{-# INLINE enlarge #-}
