{-# LANGUAGE CPP, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.NonEmpty
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Stream.NonEmpty (
   -- * The type of streams
     Stream(..)
   -- * Basic functions
   , head
   , tail
   , tails
   , inits1
   , unfold
  ) where

import Prelude hiding (head)

import Control.Applicative
import Control.Comonad
import Control.Comonad.Apply
import Control.Monad
import Data.Functor.Apply
import Data.Foldable
import Data.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

infixl 5 :|, <|

data Stream a = a :| [a] deriving 
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Functor Stream where
  fmap f (a :| as) = f a :| fmap f as
  b <$ (_ :| as)   = b   :| (b <$ as)

instance FunctorApply Stream where
  (<.>) = ap

instance Applicative Stream where
  pure a = a :| []
  (<*>) = ap

instance Monad Stream where
  return a = a :| []
  (a :| as) >>= f 
    | b :| bs <- f m
    , bs' = as >>= toList . f
    = b :| (bs ++ bs')

instance Traversable Stream where
  traverse f (a :| as) = (:|) <$> f a <*> traverse f as

instance Foldable Stream where
  foldr f z (a :| as) = f a (foldr f z as)
  foldl f x (a :| as) = foldl f (f a x) as 
  foldl1 f z (a :| as) = foldl f a as
  foldMap f (a :| as) = f a `mappend` foldMap f as
  fold (m :| ms) = m `mappend` fold ms

instance Foldable1 Stream

instance Traversable1 Stream where
  traverse1 f (a :| as) = (:|) <$> f a <.> runMaybeApply (traverse (pure . f) as)
  sequence1 (a :| as) = (:|) <$> a <.> runMaybeApply (traverse pure as)
  
instance Semigroup Stream where
  (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

head :: Stream a -> a
head (a :| _) = a

tail :: Stream a -> [a]
tail (_ :| as) = a

(<|) :: a -> Stream a -> Stream a 
a <| (b :| bs) = a :| b : bs

fromList :: [a] -> Stream a 
fromList (a:as) = a :| as
fromList [] = error "Stream.fromList: empty list"

toList :: Stream a -> [a]
toList (a :| as) = a : as

lift :: ([a] -> [b]) -> Stream a -> Stream a
lift f = fromList . f . toList 

reverse :: Stream a -> Stream a
reverse = lift List.reverse

sort :: Stream a -> Stream a 
sort = lift List.sort
