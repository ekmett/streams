{-# LANGUAGE CPP, PatternGuards, UndecidableInstances #-}
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
   , head   -- Stream f a -> a
   , tail   -- Stream f a -> f (Stream f a)
   , tails  -- Stream f a -> Stream f (Stream f a)
   , inits1 -- Stream f a -> Stream f (NonEmpty a)
   , unfold -- 
  ) where

import Prelude hiding (head, tail)

import Control.Applicative
import Control.Comonad
import Control.Comonad.Apply
import Control.Monad
import Data.Functor.Apply
import Data.Stream.NonEmpty hiding (tail, tails, unfold, head)
import qualified Data.Stream.NonEmpty as NonEmpty

#ifdef GHC_TYPEABLE
import Data.Data
#endif

infixr 5 :<

data Stream f a = a :< f (Stream f a)

head :: Stream f a -> a
head (a :< _) = a
{-# INLINE head #-}

tail :: Stream f a -> f (Stream f a)
tail (_ :< as) = as
{-# INLINE tail #-}

tails :: Functor f => Stream f a -> Stream f (Stream f a)
tails = duplicate
{-# INLINE tails #-}

-- | equivalent to inits sans the initial [] context
inits1 :: Functor f => Stream f a -> Stream f (NonEmpty a)
inits1 (a :< as) = (a :| []) :< (fmap (NonEmpty.cons a) . inits1 <$> as)

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

instance (Show (f (Stream f a)), Show a) => Show (Stream f a) where
  showsPrec d (a :< as) = showParen (d > 5) $ 
    showsPrec 6 a . showString " :< " . showsPrec 5 as

instance (Eq (f (Stream f a)), Eq a) => Eq (Stream f a) where
  a :< as == b :< bs = a == b && as == bs

instance (Ord (f (Stream f a)), Ord a) => Ord (Stream f a) where
  compare (a :< as) (b :< bs) = case compare a b of
    LT -> LT
    EQ -> compare as bs
    GT -> GT

#ifdef GHC_TYPEABLE

instance (Typeable1 f) => Typeable1 (Stream f) where
  typeOf1 dfa = mkTyConApp streamTyCon [typeOf1 (f dfa)]
    where
      f :: Stream f a -> f a
      f = undefined

instance (Typeable1 f, Typeable a) => Typeable (Stream f a) where
  typeOf = typeOfDefault

streamTyCon :: TyCon
streamTyCon = mkTyCon "Data.Stream.Branching.Stream"
{-# NOINLINE streamTyCon #-}

instance
  ( Typeable1 f
  , Data (f (Stream f a))
  , Data a
  ) => Data (Stream f a) where
    gfoldl f z (a :< as) = z (:<) `f` a `f` as
    toConstr _ = streamConstr
    gunfold k z c = case constrIndex c of
        1 -> k (k (z (:<)))
        _ -> error "gunfold"
    dataTypeOf _ = streamDataType
    dataCast1 f = gcast1 f

streamConstr :: Constr
streamConstr = mkConstr streamDataType ":<" [] Infix
{-# NOINLINE streamConstr #-}

streamDataType :: DataType
streamDataType = mkDataType "Data.Stream.Branching.Stream" [streamConstr]
{-# NOINLINE streamDataType #-}

#endif
