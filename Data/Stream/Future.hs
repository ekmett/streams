{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Future
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------

module Data.Stream.Future where

import Control.Applicative
import Control.Applicative.Alt
import Control.Comonad
import Control.Comonad.Apply
import Data.Foldable
import Data.Functor.Alt
import Data.Traversable
import Data.Semigroup hiding (Last)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Data

data Future a = a :< Future a | Last a
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Functor Future where
  fmap f (a :< as) = f a :< fmap f as
  fmap f (Last a)  = Last (f a)
  b <$ (_ :< as) = b :< (b <$ as)
  b <$ _         = Last b
  
instance Foldable Future where 
  foldMap = foldMapDefault
  
instance Traversable Future where
  traverse f (Last a)  = Last <$> f a
  traverse f (a :< as) = (:<) <$> f a <*> traverse f as

instance Foldable1 Future

instance Traversable1 Future where
  traverse1 f (Last a)  = Last <$> f a
  traverse1 f (a :< as) = (:<) <$> f a <.> traverse1 f as

instance Comonad Future where
  extract (Last a) = a
  extract (a :< _) = a
  extend f w@(_ :< as) = f w :< extend f as
  extend f w@(Last _)  = Last (f w)

instance FunctorApply Future where
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
  
instance FunctorAlt Future where
  Last a    <!> bs = a :< bs
  (a :< as) <!> bs = a :< (as <!> bs)

instance Semigroup (Future a) where
  (<>) = (<!>)
  
instance ComonadApply Future

instance Applicative Future where
  pure = Last
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance ApplicativeAlt Future
