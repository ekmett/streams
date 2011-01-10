{-# LANGUAGE DeriveDataTypeable, PatternGuards, UndecidableInstances, StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Branching
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Stream.Branching (
   -- * The type of streams
     Stream(..)
   -- * Basic functions
   , head
   , tail
   , tails
   , inits1
   , unfold
  ) where

import Prelude hiding (head, tail)

import Control.Applicative
import Control.Comonad
import Control.Comonad.Apply
import Control.Monad
import Data.Functor.Apply

infixl 5 :<

data Stream f a = a :< f (Stream f a)

deriving instance (Show (f (Stream f a)), Show a) => Show (Stream f a)
deriving instance (Eq (f (Stream f a)), Eq a) => Eq (Stream f a)
deriving instance (Ord (f (Stream f a)), Ord a) => Ord (Stream f a)

head :: Stream f a -> a
head (a :< _) = a

tail :: Stream f a -> f (Stream f a)
tail (_ :< as) = as


tails :: Functor f => Stream f a -> Stream f (Stream f a)
tails = duplicate

inits1 :: Functor f => Stream f a -> Stream f [a]
inits1 (a :< as) = [a] :< (fmap (a :) . inits1 <$> as)

instance Functor f => Functor (Stream f) where
  fmap f (a :< as) = f a :< fmap (fmap f) as
  b <$ (_ :< as) = b :< fmap (b <$) as

instance Functor f => Comonad (Stream f) where
  extract (a :< _) = a
  extend f w = f w :< fmap (extend f) (tail w)
  duplicate w = w :< fmap duplicate (tail w)

instance FunctorApply f => FunctorApply (Stream f) where
  (f :< fs) <.> (a :< as) = f a :< ((<.>) <$> fs <.> as)
  (f :< fs) <.  (_ :< as) = f :< ((<. ) <$> fs <.> as)
  (_ :< fs)  .> (a :< as) = a :< (( .>) <$> fs <.> as)

instance FunctorApply f => ComonadApply (Stream f)

instance Applicative f => Applicative (Stream f) where
  pure a = as where as = a :< pure as
  (f :< fs) <*> (a :< as) = f a :< ((<*>) <$> fs <*> as)
  (f :< fs) <*  (_ :< as) = f :< ((<* ) <$> fs <*> as)
  (_ :< fs)  *> (a :< as) = a :< (( *>) <$> fs <*> as)

unfold :: Functor f => (b -> (a, f b)) -> b -> Stream f a
unfold f c | (x, d) <- f c = x :< fmap (unfold f) d

-- unsound

{-
munfold :: Monad m => (b -> (a, m b)) -> b -> Stream m a
munfold f c | (x, d) <- f c = x :< liftM (munfold f) d

mfmap :: Monad m => (a -> b) -> Stream m a -> Stream m b
mfmap f (a :< as) = f a :< liftM (mfmap f) as

instance Monad m => Monad (Stream m) where
  return a = as where as = a :< return as
  m >>= f = munfold (\(bs :< bss) -> (head bs, bss >>= tails)) (mfmap f m)
-}
