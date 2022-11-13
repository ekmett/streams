{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Infinite
-- Copyright   :  (C) 2011 Edward Kmett,
--                (C) 2007-2010 Wouter Swierstra, Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable (Haskell 2010)
--
----------------------------------------------------------------------------
module Data.Stream.Infinite (
   -- * The type of streams
     Stream(..)
   -- * Basic functions
   , tail   -- :: Stream a -> Stream a
   , inits  -- :: Stream a -> Stream [a]
   , prepend -- :: [a] -> Stream a -> Stream a
   , concat -- :: Stream [a] -> Stream a
   -- * Stream transformations
   , intersperse -- :: a -> Stream a -> Stream
   , interleave  -- :: Stream a -> Stream a -> Stream a
   , scanl       -- :: (b -> a -> b) -> b -> Stream a -> Stream b
   , scanl'      -- :: (b -> a -> b) -> b -> Stream a -> Stream b
   , scanl1      -- :: (a -> a -> a) -> Stream a -> Stream a
   , scanl1'     -- :: (a -> a -> a) -> Stream a -> Stream a
   , transpose   -- :: Stream (Stream a) -> Stream (Stream a)
   -- * Building streams
   , iterate     -- :: (a -> a) -> a -> Stream a
   , cycle       -- :: NonEmpty a -> Stream a
   , unfold      -- :: (a -> (b, a)) -> a -> Stream b
   -- * Extracting sublists
   , take        -- :: Int -> Stream a -> [a]
   , drop        -- :: Int -> Stream a -> Stream a
   , splitAt     -- :: Int -> Stream a -> ([a],Stream a)
   , takeWhile   -- :: (a -> Bool) -> Stream a -> [a]
   , dropWhile   -- :: (a -> Bool) -> Stream a -> Stream a
   , span        -- :: (a -> Bool) -> Stream a -> ([a], Stream a)
   , break       -- :: (a -> Bool) -> Stream a -> ([a], Stream a)
   , filter      -- :: (a -> Bool) -> Stream a -> Stream a
   , partition   -- :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
   , group       -- :: (a -> Bool) -> Stream a -> Stream (NonEmpty a)
   , groupBy     -- :: (a -> a -> Bool) -> Stream a -> Stream (NonEmpty a)
   -- * Sublist predicates
   , isPrefixOf  -- :: [a] -> Stream a -> Bool
   -- * Indexing streams
   , (!!)        -- :: Int -> Stream a -> a
   , elemIndex   -- :: Eq a => a -> Stream a -> Int
   , elemIndices -- :: Eq a => a -> Stream a -> Stream Int
   , findIndex   -- :: (a -> Bool) -> Stream a -> Int
   , findIndices -- :: (a -> Bool) -> Stream a -> Stream Int
   -- * Zipping and unzipping streams
   , zip         -- :: Stream a -> Stream b -> Stream (a, b)
   , zipWith     -- :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
   , unzip       -- :: Functor f => f (a, b) -> (f a, f b)
   -- * Functions on streams of characters
   , words       -- :: Stream Char -> Stream String
   , unwords     -- :: Stream String -> Stream Char
   , lines       -- :: Stream Char -> Stream String
   , unlines     -- :: Stream String -> Stream Char
   ) where

import Prelude hiding
  ( tail, map, scanr, scanr1, scanl, scanl1
  , iterate, take, drop, takeWhile
  , dropWhile, repeat, cycle, filter
  , (!!), zip, unzip, zipWith, words
  , unwords, lines, unlines, break, span
  , splitAt, foldr, concat
  )

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif
import Control.Comonad
import Data.Char (isSpace)
import Data.Data
import Data.Functor.Apply
import Data.Functor.Extend
import Data.Functor.Rep
#if !(MIN_VERSION_base(4,8,0))
import Data.Semigroup
import Data.Traversable
#endif
import Data.Foldable hiding (concat)
import Data.Distributive
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Boring (Boring (..), Absurd (..))

data Stream a = a :> Stream a deriving
  ( Show
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

infixr 5 :>

instance Functor Stream where
  fmap f (a :> as) = f a :> fmap f as
  b <$ _ = pure b

instance Distributive Stream where
  distribute w = fmap extract w :> distribute (fmap tail w)

instance Representable Stream where
  type Rep Stream = Int
  tabulate f      = unfold (\i -> (,) (f i) $! (i + 1)) 0
  index (x :> xs) n
    | n == 0    = x
    | n > 0     = xs !! (n - 1)
    | otherwise = error "Stream.!! negative argument"

-- | Extract the first element of the stream.
head :: Stream a -> a
head (x :> _) = x

-- | @since 3.3.1
instance Boring a => Boring (Stream a) where
  boring = pure boring

-- | @since 3.3.1
instance Absurd a => Absurd (Stream a) where
  absurd = absurd . extract

-- | Extract the sequence following the head of the stream.
tail :: Stream a -> Stream a
tail (_ :> as) = as
{-# INLINE tail #-}

instance Extend Stream where
  duplicated = duplicate
  extended = extend

instance Comonad Stream where
  duplicate w = w :> duplicate (tail w)
  extend f w = f w :> extend f (tail w)
  extract (a :> _) = a

instance Apply Stream where
  (f :> fs) <.> (a :> as) = f a :> (fs <.> as)
  as        <.  _         = as
  _          .> bs        = bs

instance ComonadApply Stream where
  (f :> fs) <@> (a :> as) = f a :> (fs <@> as)
  as        <@  _         = as
  _          @> bs        = bs

instance Applicative Stream where
  pure a = as where as = a :> as
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance Foldable Stream where
  fold (m :> ms) = m `mappend` fold ms
  foldMap f (a :> as) = f a `mappend` foldMap f as
  foldr f0 _ = go f0 where go f (a :> as) = f a (go f as)
#if __GLASGOW_HASKELL__ > 710
  length _ = error "infinite length"
  null _ = False
#endif

instance Traversable Stream where
  traverse f ~(a :> as) = (:>) <$> f a <*> traverse f as

instance Foldable1 Stream

instance Traversable1 Stream where
  traverse1 f ~(a :> as) = (:>) <$> f a <.> traverse1 f as
  sequence1 ~(a :> as) = (:>) <$> a <.> sequence1 as

-- | The unfold function is similar to the unfold for lists. Note
-- there is no base case: all streams must be infinite.
unfold :: (a -> (b, a)) -> a -> Stream b
unfold f c | (x, d) <- f c = x :> unfold f d

instance Monad Stream where
  m >>= f = unfold (\(bs :> bss) -> (extract bs, tail <$> bss)) (fmap f m)
#if !(MIN_VERSION_base(4,11,0))
  return = pure
  _ >> bs = bs
#endif

-- | Interleave two Streams @xs@ and @ys@, alternating elements
-- from each list.
--
-- > [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
interleave :: Stream a -> Stream a -> Stream a
interleave ~(x :> xs) ys = x :> interleave ys xs

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@.
--
-- Note that this 'inits' is lazier then @Data.List.inits@:
--
-- > inits _|_ = [] ::: _|_
--
-- while for @Data.List.inits@:
--
-- > inits _|_ = _|_
inits :: Stream a -> Stream [a]
inits xs = [] :> ((extract xs :) <$> inits (tail xs))

-- | Prepend a list to a stream.
prepend :: Foldable f => f a -> Stream a -> Stream a
prepend xs ys = foldr (:>) ys xs

-- | Flatten a stream of lists into a stream.
concat :: Foldable f => Stream (f a) -> Stream a
concat = foldr prepend undefined

-- | @'intersperse' y xs@ creates an alternating stream of
-- elements from @xs@ and @y@.
intersperse :: a -> Stream a -> Stream a
intersperse y ~(x :> xs) = x :> y :> intersperse y xs

-- | 'scanl' yields a stream of successive reduced values from:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl :: (a -> b -> a) -> a -> Stream b -> Stream a
scanl f z ~(x :> xs) = z :> scanl f (f z x) xs

-- | 'scanl' yields a stream of successive reduced values from:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl' :: (a -> b -> a) -> a -> Stream b -> Stream a
scanl' f z ~(x :> xs) = z :> (scanl' f $! f z x) xs

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (a -> a -> a) -> Stream a -> Stream a
scanl1 f ~(x :> xs) = scanl f x xs

-- | @scanl1'@ is a strict 'scanl' that has no starting value.
scanl1' :: (a -> a -> a) -> Stream a -> Stream a
scanl1' f ~(x :> xs) = scanl' f x xs

-- | 'transpose' computes the transposition of a stream of streams.
transpose :: Stream (Stream a) -> Stream (Stream a)
transpose ~((x :> xs) :> yss) =
  (x :> (extract <$> yss)) :> transpose (xs :> (tail <$> yss))

-- | @'iterate' f x@ produces the infinite sequence
-- of repeated applications of @f@ to @x@.
--
-- > iterate f x = [x, f x, f (f x), ..]
iterate :: (a -> a) -> a -> Stream a
iterate f x = x :> iterate f (f x)

-- | @'cycle' xs@ returns the infinite repetition of @xs@:
--
-- > cycle [1,2,3] = Cons 1 (Cons 2 (Cons 3 (Cons 1 (Cons 2 ...
cycle :: NonEmpty a -> Stream a
cycle xs = ys where ys = foldr (:>) ys xs

-- | @'take' n xs@ returns the first @n@ elements of @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
take :: Int -> Stream a -> [a]
take n ~(x :> xs)
  | n == 0 = []
  | n > 0 = x : take (n - 1) xs
  | otherwise = error "Stream.take: negative argument"

-- | @'drop' n xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
drop :: Int -> Stream a -> Stream a
drop n xs
  | n == 0 = xs
  | n > 0 = drop (n - 1) (tail xs)
  | otherwise = error "Stream.drop: negative argument"

-- | @'splitAt' n xs@ returns a pair consisting of the prefix of
-- @xs@ of length @n@ and the remaining stream immediately following
-- this prefix.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
splitAt :: Int -> Stream a -> ([a],Stream a)
splitAt n xs
  | n == 0 = ([],xs)
  | n > 0, (prefix, rest) <- splitAt (n - 1) (tail xs) = (extract xs : prefix, rest)
  | otherwise = error "Stream.splitAt: negative argument"

-- | @'takeWhile' p xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (x :> xs)
  | p x = x : takeWhile p xs
  | otherwise = []

-- | @'dropWhile' p xs@ returns the suffix remaining after
-- @'takeWhile' p xs@.
--
-- /Beware/: this function may diverge if every element of @xs@
-- satisfies @p@, e.g.  @dropWhile even (repeat 0)@ will loop.
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p ~(x :> xs)
  | p x = dropWhile p xs
  | otherwise = x :> xs

-- | @'span' p xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
span :: (a -> Bool) -> Stream a -> ([a], Stream a)
span p xxs@(x :> xs)
  | p x, (ts, fs) <- span p xs = (x : ts, fs)
  | otherwise = ([], xxs)

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
break :: (a -> Bool) -> Stream a -> ([a], Stream a)
break p = span (not . p)

-- | @'filter' p xs@, removes any elements from @xs@ that do not satisfy @p@.
--
-- /Beware/: this function may diverge if there is no element of
-- @xs@ that satisfies @p@, e.g.  @filter odd (repeat 0)@ will loop.
filter :: (a -> Bool) -> Stream a -> Stream a
filter p ~(x :> xs)
  | p x       = x :> filter p xs
  | otherwise = filter p xs

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of streams. The first stream corresponds
-- to the elements of @xs@ for which @p@ holds; the second stream
-- corresponds to the elements of @xs@ for which @p@ does not hold.
--
-- /Beware/: One of the elements of the tuple may be undefined. For
-- example, @fst (partition even (repeat 0)) == repeat 0@; on the
-- other hand @snd (partition even (repeat 0))@ is undefined.
partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
partition p ~(x :> xs)
  | p x = (x :> ts, fs)
  | otherwise = (ts, x :> fs)
  where (ts, fs) = partition p xs

-- | The 'group' function takes a stream and returns a stream of
-- lists such that flattening the resulting stream is equal to the
-- argument.  Moreover, each sublist in the resulting stream
-- contains only equal elements.  For example,
--
-- > group $ cycle "Mississippi" = "M" ::: "i" ::: "ss" ::: "i" ::: "ss" ::: "i" ::: "pp" ::: "i" ::: "M" ::: "i" ::: ...
group :: Eq a => Stream a -> Stream (NonEmpty a)
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> Stream a -> Stream (NonEmpty a)
groupBy eq ~(x :> ys)
  | (xs, zs) <- span (eq x) ys
  = (x :| xs) :> groupBy eq zs

-- | The 'isPrefix' function returns @True@ if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> Stream a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (x :> xs)
  | y == x    = isPrefixOf ys xs
  | otherwise = False

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
--
-- /Beware/: passing a negative integer as the first argument will cause
-- an error.
(!!) :: Stream a -> Int -> a
(!!) = index

-- | The 'elemIndex' function returns the index of the first element
-- in the given stream which is equal (by '==') to the query element,
--
-- /Beware/: @'elemIndex' x xs@ will diverge if none of the elements
-- of @xs@ equal @x@.
elemIndex :: Eq a => a -> Stream a -> Int
elemIndex x = findIndex (\y -> x == y)

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
--
-- /Beware/: 'elemIndices' @x@ @xs@ will diverge if any suffix of
-- @xs@ does not contain @x@.
elemIndices :: Eq a => a -> Stream a -> Stream Int
elemIndices x = findIndices (x==)

-- | The 'findIndex' function takes a predicate and a stream and returns
-- the index of the first element in the stream that satisfies the predicate,
--
-- /Beware/: 'findIndex' @p@ @xs@ will diverge if none of the elements of
-- @xs@ satisfy @p@.
findIndex :: (a -> Bool) -> Stream a -> Int
findIndex p = indexFrom 0
    where
    indexFrom ix (x :> xs)
      | p x       = ix
      | otherwise = (indexFrom $! (ix + 1)) xs

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending
-- order.
--
-- /Beware/: 'findIndices' @p@ @xs@ will diverge if all the elements
-- of any suffix of @xs@ fails to satisfy @p@.
findIndices :: (a -> Bool) -> Stream a -> Stream Int
findIndices p = indicesFrom 0 where
  indicesFrom ix (x :> xs)
    | p x = ix :> ixs
    | otherwise = ixs
    where ixs = (indicesFrom $! (ix+1)) xs

-- | The 'zip' function takes two streams and returns a list of
-- corresponding pairs.
zip :: Stream a -> Stream b -> Stream (a,b)
zip ~(x :> xs) ~(y :> ys) = (x,y) :> zip xs ys

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the functions, the elements are combined using the function
-- passed as the first argument to 'zipWith'.
zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f ~(x :> xs) ~(y :> ys) = f x y :> zipWith f xs ys

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Stream (a,b) -> (Stream a, Stream b)
unzip xs = (fst <$> xs, snd <$> xs)

-- | The 'words' function breaks a stream of characters into a
-- stream of words, which were delimited by white space.
--
-- /Beware/: if the stream of characters @xs@ does not contain white
-- space, accessing the tail of @words xs@ will loop.
words :: Stream Char -> Stream String
words xs | (w, ys) <- break isSpace xs = w :> words ys

-- | The 'unwords' function is an inverse operation to 'words'. It
-- joins words with separating spaces.
unwords :: Stream String -> Stream Char
unwords ~(x :> xs) = foldr (:>) (' ' :> unwords xs) x

-- | The 'lines' function breaks a stream of characters into a list
-- of strings at newline characters. The resulting strings do not
-- contain newlines.
--
-- /Beware/: if the stream of characters @xs@ does not contain
-- newline characters, accessing the tail of @lines xs@ will loop.
lines :: Stream Char -> Stream String
lines xs | (l, ys) <- break (== '\n') xs = l :> lines (tail ys)

-- | The 'unlines' function is an inverse operation to 'lines'. It
-- joins lines, after appending a terminating newline to each.
unlines :: Stream String -> Stream Char
unlines ~(x :> xs) = foldr (:>) ('\n' :> unlines xs) x
