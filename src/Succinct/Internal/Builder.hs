{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Succinct.Internal.Builder
  ( Builder(..)
  , Buildable(..)
  ) where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import Data.Vector.Internal.Check as Ck

#define INTERNAL_CHECK(f) Ck.f __FILE__ __LINE__ Ck.Internal

data family Builder (t :: * -> *) :: * -> * -> *

class Buildable t a where
  new    :: PrimMonad m => m (Builder t (PrimState m) a)
  snoc   :: PrimMonad m => Builder t (PrimState m) a -> a -> m (Builder t (PrimState m) a)
  freeze :: PrimMonad m => Builder t (PrimState m) a -> m (t a)

  unsafeFreeze :: PrimMonad m => Builder t (PrimState m) a -> m (t a)
  unsafeFreeze = freeze

newtype instance Builder [] s a = BuildList ([a] -> [a])

instance Buildable [] a where
  new = return (BuildList id)
  {-# INLINE new #-}
  snoc (BuildList f) a = return $ BuildList (f . (a:))
  {-# INLINE snoc #-}
  freeze (BuildList f) = return $ f []
  {-# INLINE freeze #-}

data instance Builder V.Vector s a = BuildVector {-# UNPACK #-} !Int !(VM.MVector s a)

instance Buildable V.Vector a where
  new = BuildVector 0 `liftM` VM.unsafeNew 0
  {-# INLINE new #-}
  snoc (BuildVector i v) x = do
    v' <- unsafeAppend1 v i x
    return $ BuildVector (i + 1) v'
  {-# INLINE snoc #-}
  freeze (BuildVector n v)
    = V.freeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (VM.length v)
    $ VM.unsafeSlice 0 n v
  {-# INLINE freeze #-}
  unsafeFreeze (BuildVector n v)
    = V.unsafeFreeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (VM.length v)
    $ VM.unsafeSlice 0 n v
  {-# INLINE unsafeFreeze #-}

-- * Vector utilities

data instance Builder U.Vector s a = BuildUVector {-# UNPACK #-} !Int !(UM.MVector s a)

instance U.Unbox a => Buildable U.Vector a where
  new = BuildUVector 0 `liftM` UM.unsafeNew 0
  {-# INLINE new #-}
  snoc (BuildUVector i v) x = do
    v' <- unsafeAppend1 v i x
    return $ BuildUVector (i + 1) v'
  {-# INLINE snoc #-}
  freeze (BuildUVector n v)
    = U.freeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (UM.length v)
    $ UM.unsafeSlice 0 n v
  {-# INLINE freeze #-}
  unsafeFreeze (BuildUVector n v)
    = U.unsafeFreeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (UM.length v)
    $ UM.unsafeSlice 0 n v
  {-# INLINE unsafeFreeze #-}

data instance Builder P.Vector s a = BuildPVector {-# UNPACK #-} !Int !(PM.MVector s a)

instance P.Prim a => Buildable P.Vector a where
  new = BuildPVector 0 `liftM` PM.unsafeNew 0
  {-# INLINE new #-}
  snoc (BuildPVector i v) x = do
    v' <- unsafeAppend1 v i x
    return $ BuildPVector (i + 1) v'
  {-# INLINE snoc #-}
  freeze (BuildPVector n v)
    = P.freeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (PM.length v)
    $ PM.unsafeSlice 0 n v
  {-# INLINE freeze #-}
  unsafeFreeze (BuildPVector n v)
    = P.unsafeFreeze
    $ INTERNAL_CHECK(checkSlice) "runBuilder" 0 n (PM.length v)
    $ PM.unsafeSlice 0 n v
  {-# INLINE unsafeFreeze #-}


unsafeAppend1 :: (PrimMonad m, GM.MVector v a)
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

enlarge_delta :: GM.MVector v a => v s a -> Int
enlarge_delta v = max (GM.length v) 1

-- | Grow a vector logarithmically
enlarge :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
enlarge v = GM.unsafeGrow v (enlarge_delta v)
{-# INLINE enlarge #-}
