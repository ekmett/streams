{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Future
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------

module Data.Stream.Future
  ( Future(..)
  , tail
  , length
  , index
  ) where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (tail)
#else
import Control.Applicative
import Prelude hiding (tail, length)
import Data.Foldable
#endif

import Control.Comonad
import Data.Functor.Alt
import Data.Functor.Extend
import Data.Traversable
import Data.Semigroup hiding (Last)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts as Exts
#endif

infixr 5 :<

data Future a = Last a | a :< Future a deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

#if __GLASGOW_HASKELL__ < 710
length :: Future a -> Int
length = go 1
  where
    go !n (Last _)  = n
    go !n (_ :< as) = go (n + 1) as
{-# INLINE length #-}
#endif

tail :: Future a -> Maybe (Future a)
tail (Last _) = Nothing
tail (_ :< as) = Just as
{-# INLINE tail #-}

index :: Int -> Future a -> a
index n aas
  | n < 0 = error "index: negative index"
  | n == 0 = extract aas
  | otherwise = case aas of
    Last _ -> error "index: out of range"
    _ :< as -> index (n - 1) as

instance Functor Future where
  fmap f (a :< as) = f a :< fmap f as
  fmap f (Last a)  = Last (f a)
  b <$ (_ :< as) = b :< (b <$ as)
  b <$ _         = Last b

instance Foldable Future where
  foldMap = foldMapDefault
#if __GLASGOW_HASKELL__ >= 710
  length = go 1
    where
      go !n (Last _)  = n
      go !n (_ :< as) = go (n + 1) as
  {-# INLINE length #-}
  null _ = False
#endif

instance Traversable Future where
  traverse f (Last a)  = Last <$> f a
  traverse f (a :< as) = (:<) <$> f a <*> traverse f as

instance Foldable1 Future

instance Traversable1 Future where
  traverse1 f (Last a)  = Last <$> f a
  traverse1 f (a :< as) = (:<) <$> f a <.> traverse1 f as

instance Extend Future where
  extended = extend

instance Comonad Future where
  extract (Last a) = a
  extract (a :< _) = a

  duplicate w@(_ :< as) = w :< duplicate as
  duplicate w@(Last _)  = Last w

  extend f w@(_ :< as) = f w :< extend f as
  extend f w@(Last _)  = Last (f w)

instance Apply Future where
  Last f    <.> Last a    = Last (f a)
  (f :< _)  <.> Last a    = Last (f a)
  Last f    <.> (a :< _ ) = Last (f a)
  (f :< fs) <.> (a :< as) = f a :< (fs <.> as)

  Last a    <. _         = Last a
  (a :< _ ) <. Last _    = Last a
  (a :< as) <. (_ :< bs) = a :< (as <. bs)

  _          .> Last b   = Last b
  Last _     .> (b :< _) = Last b
  (_ :< as)  .> (b :< bs) = b :< (as .> bs)

instance ComonadApply Future where
  (<@>) = (<.>)

instance Alt Future where
  Last a    <!> bs = a :< bs
  (a :< as) <!> bs = a :< (as <!> bs)

instance Semigroup (Future a) where
  (<>) = (<!>)

instance Applicative Future where
  pure = Last
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

#if MIN_VERSION_base(4,7,0)
instance Exts.IsList (Future a) where
  type Item (Future a) = a

  toList (Last a) = [a]
  toList (a :< as) = a : toList as

  fromList [] = error "Future.fromList: empty list"
  fromList (x:xs) = go x xs where
    go y [] = Last y
    go y (z:zs) = y :< go z zs

  fromListN _ = Exts.fromList
#endif
