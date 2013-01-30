{-# LANGUAGE CPP, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Supply
-- Copyright   :  (C) 2008-2011 Edward Kmett,
--                (C) 2008 Iavor S. Diatchki
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This library can be used to generate values (for example, new names)
-- without the need to thread state.  This means that functions that
-- need to generate new values only need a supply object as an argument,
-- and they do not need to return a new supply object as a result.
-- This decreases the number of data-dependencies in a program, which
-- makes it easier to exploit parallelism.
--
-- The technique for generating new values is based on the paper
-- ''On Generating Unique Names'' by Lennart Augustsson, Mikael Rittri, 
-- and Dan Synek.
----------------------------------------------------------------------------
module Data.Stream.Supply
  ( Supply
  , newSupply
  , newEnumSupply
  , newNumSupply
  , newDupableSupply
  , newDupableEnumSupply
  , newDupableNumSupply
  , leftSupply
  , rightSupply
  , split
  , splits
  , splitSkew
  , split2
  , split3
  , split4
  ) where

import Control.Applicative
import Control.Comonad
import Data.Functor.Apply
import Data.Functor.Extend
import Data.Foldable
import Data.IORef(newIORef, atomicModifyIORef)
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Stream.Infinite
import qualified Data.Stream.Infinite.Skew as Skew

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

#if __GLASGOW_HASKELL__ >= 608 
import GHC.IO(unsafeDupableInterleaveIO)
#else
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#endif

data Supply a = Supply a (Supply a) (Supply a) deriving 
  ( Show, Read, Eq, Ord
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Functor Supply where
  fmap f (Supply a l r) = Supply (f a) (fmap f l) (fmap f r)
  a <$ _ = pure a

instance Extend Supply where
  extended f s@(Supply _ l r) = Supply (f s) (extended f l) (extended f r)
  duplicated s@(Supply _ l r) = Supply s (duplicated l) (duplicated r)

instance Comonad Supply where
  extend f s@(Supply _ l r) = Supply (f s) (extend f l) (extend f r)
  duplicate s@(Supply _ l r) = Supply s (duplicate l) (duplicate r)
  extract (Supply a _ _) = a

instance Apply Supply where
  Supply f fl fr <.> Supply a al ar = Supply (f a) (fl <.> al) (fr <.> ar)
  a <. _ = a
  _ .> a = a

instance Applicative Supply where
  pure a = as where as = Supply a as as
  Supply f fl fr <*> Supply a al ar = Supply (f a) (fl <*> al) (fr <*> ar)
  a <* _ = a
  _ *> a = a

instance Foldable Supply where
  foldMap f (Supply a l r) = f a `mappend` foldMap f l `mappend` foldMap f r

instance Foldable1 Supply where
  foldMap1 f (Supply a l r) = f a <> foldMap1 f l <> foldMap1 f r

instance Traversable Supply where
  traverse f (Supply a l r) = Supply <$> f a <*> traverse f l <*> traverse f r

instance Traversable1 Supply where
  traverse1 f (Supply a l r) = Supply <$> f a <.> traverse1 f l <.> traverse1 f r
  
leftSupply :: Supply a -> Supply a
leftSupply (Supply _ l _) = l

rightSupply :: Supply a -> Supply a
rightSupply (Supply _ _ r) = r

-- unfoldsW :: (Comonad w, Functor f) => (w a -> (b, f a)) -> w a -> StreamT f w b
newSupply :: (a -> a) -> a -> IO (Supply a)
newSupply f x = gen =<< newIORef x
  where gen r = unsafeInterleaveIO $
          Supply <$> unsafeInterleaveIO (atomicModifyIORef r update) 
                 <*> gen r 
                 <*> gen r
        update a = b `seq` (b, a) where b = f a
{-# INLINE newSupply #-}

newDupableSupply :: (a -> a) -> a -> IO (Supply a)
newDupableSupply f x = gen =<< newIORef x
  where gen r = unsafeDupableInterleaveIO $
          Supply <$> unsafeDupableInterleaveIO (atomicModifyIORef r update)
                 <*> gen r
                 <*> gen r
        update a = b `seq` (b, a) where b = f a
{-# INLINE newDupableSupply #-}

newEnumSupply :: Enum a => IO (Supply a)
newEnumSupply = newSupply succ (toEnum 0)
{-# SPECIALIZE newEnumSupply :: IO (Supply Int) #-}

newNumSupply :: Num a => IO (Supply a)
newNumSupply = newSupply (1+) 0
{-# SPECIALIZE newNumSupply :: IO (Supply Int) #-}

newDupableEnumSupply :: Enum a => IO (Supply a)
newDupableEnumSupply = newSupply succ (toEnum 0)
{-# SPECIALIZE newEnumSupply :: IO (Supply Int) #-}

newDupableNumSupply :: Num a => IO (Supply a)
newDupableNumSupply = newSupply (1+) 0
{-# SPECIALIZE newNumSupply :: IO (Supply Int) #-}

split :: Supply a -> Stream (Supply a)
split (Supply _ l r) = l :> split r

splits :: Integral b => Supply a -> b -> Supply a
splits (Supply _ l r) n = case n `quotRem` 2 of
  (0,0)  -> leftSupply l
  (q,-1) -> splits (rightSupply l) q
  (q,0)  -> splits (leftSupply r) q
  (q,1)  -> splits (rightSupply r) q
  (_,_)  -> error "quotRem: impossible result"
{-# SPECIALIZE splits :: Supply a -> Int -> Supply a #-}
{-# SPECIALIZE splits :: Supply a -> Integer -> Supply a #-}

splitSkew :: Supply a -> Skew.Stream (Supply a)
splitSkew = Skew.tabulate . splits

split2 :: Supply a -> (Supply a, Supply a)
split2 (Supply _ l r) = (l, r)

split3 :: Supply a -> (Supply a, Supply a, Supply a)
split3 (Supply _ a (Supply _ b c)) = (a, b, c)

split4 :: Supply a -> (Supply a, Supply a, Supply a, Supply a)
split4 (Supply _ (Supply _ a b) (Supply _ c d)) = (a, b, c, d)
