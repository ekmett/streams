{-# LANGUAGE CPP, PatternGuards, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Infinite.Functional.Zipper
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This is an infinite bidirectional zipper
----------------------------------------------------------------------------
module Data.Stream.Infinite.Functional.Zipper (
   -- * The type of streams
     Zipper(..)
   , tail   -- :: Zipper a -> Zipper a
   , untail -- :: Zipper a -> Zipper a
   , intersperse -- :: a -> Zipper a -> Zipper a
   , interleave  -- :: Zipper a -> Zipper a -> Zipper a
   , transpose   -- :: Zipper (Zipper a) -> Zipper (Zipper a)
   , take        -- :: Integer -> Zipper a -> [a]
   , drop        -- :: Integer -> Zipper a -> Zipper a -- you can drop a negative number
   , splitAt     -- :: Integer -> Zipper a -> ([a],Zipper a)
   , reverse     -- :: Zipper a -> Zipper a
   , (!!)        -- :: Int -> Zipper a -> a
   , unzip       -- :: Functor f => f (a, b) -> (f a, f b)
   , toSequence  -- :: (Integer -> a) -> Zipper a
   , head
   , (<|)
   , uncons
   , takeWhile
   , dropWhile
   , span
   , break
   , isPrefixOf
   , findIndex
   , elemIndex
   , zip
   , zipWith
   ) where

import Prelude hiding
  ( head, tail, map, scanr, scanr1, scanl, scanl1
  , iterate, take, drop, takeWhile
  , dropWhile, repeat, cycle, filter
  , (!!), zip, unzip, zipWith, words
  , unwords, lines, unlines, break, span
  , splitAt, foldr, reverse
  )

#if !(MIN_VERSION_base(4,18,0))
import Control.Applicative
#endif
import Control.Comonad
#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif
import Data.Functor.Extend
import Data.Functor.Apply
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

data Zipper a = !Integer :~ !(Integer -> a)
#ifdef LANGUAGE_DeriveDataTypeable
  deriving Typeable
#endif

toSequence :: (Integer -> a) -> Zipper a
toSequence = (0 :~)

reverse :: Zipper a -> Zipper a
reverse (n :~ f) = negate n :~ f . negate

infixr 0 :~

instance Functor Zipper where
  fmap g (n :~ f) = n :~ g . f
  b <$ _ = 0 :~ const b

-- | Extract the focused element
head :: Zipper a -> a
head (n :~ f) = f n

-- | Move the head of the zipper to the right
tail :: Zipper a -> Zipper a
tail (n :~ f) = n + 1 :~ f

-- | Move the head of the zipper to the left
untail :: Zipper a -> Zipper a
untail (n :~ f) = n - 1 :~ f

-- | Cons before the head of the zipper. The head now points to the new element
(<|) :: a -> Zipper a -> Zipper a
a <| (n :~ f) = n :~ \z -> case compare z n of
  LT -> f n
  EQ -> a
  GT -> f (n - 1)

-- | Move the head of the zipper one step to the right, returning the value we move over.
uncons :: Zipper a -> (a, Zipper a)
uncons (n :~ f) = (f n, n + 1 :~ f)

instance Extend Zipper where
  duplicated (n :~ f) = n :~ (:~ f)

instance Comonad Zipper where
  duplicate (n :~ f) = n :~ (:~ f)
  extract (n :~ f) = f n

instance Apply Zipper where
  (nf :~ f) <.> (na :~ a)
    | dn <- na - nf
    = nf :~ \n -> f n (a (n + dn))
  as        <.  _         = as
  _          .> bs        = bs

instance ComonadApply Zipper where
  (<@>) = (<.>)
  (<@) = (<.)
  (@>) = (.>)


instance Applicative Zipper where
  pure = repeat
  (<*>) = (<.>)
  as <* _ = as
  _ *> bs = bs

instance Monad Zipper where
#if !(MIN_VERSION_base(4,11,0))
  return = repeat
#endif
  (z :~ ma) >>= f = z :~ \ na -> case f (ma na) of
    nb :~ mb -> mb (nb + na - z)

repeat :: a -> Zipper a
repeat a = 0 :~ const a

-- | Interleave two Zippers @xs@ and @ys@, alternating elements
-- from each list.
--
-- > [x1,x2,...] `interleave` [y1,y2,...] == [x1,y1,x2,y2,...]
-- > interleave = (<>)
interleave :: Zipper a -> Zipper a -> Zipper a
interleave = (<>)
instance Semigroup (Zipper a) where
  (n :~ a) <> (m :~ b) = 0 :~ \p -> case quotRem p 2 of
    (q, 0) -> a (n + q)
    (q, _) -> b (m + q)

-- | @'intersperse' y xs@ creates an alternating stream of
-- elements from @xs@ and @y@.
intersperse :: a -> Zipper a -> Zipper a
intersperse y z = z <> repeat y

-- | 'transpose' computes the transposition of a stream of streams.
transpose :: Zipper (Zipper a) -> Zipper (Zipper a)
transpose (n :~ f) = 0 :~ \z -> n :~ \n' -> let m :~ g = f n' in g (m + z)

take :: Integer -> Zipper a -> [a]
take n0 (m0 :~ f0)
  | n0 < 0 = error "Zipper.take: negative argument"
  | otherwise = go n0 m0 f0
  where
    go 0 !_ !_ = []
    go n  m  f = f m : go (n - 1) (m + 1) f

-- | @'drop' n xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
drop :: Integer -> Zipper a -> Zipper a
drop m (n :~ f) = m + n :~ f

-- | @'splitAt' n xs@ returns a pair consisting of the prefix of
-- @xs@ of length @n@ and the remaining stream immediately following
-- this prefix.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error if you access the taken portion
splitAt :: Integer -> Zipper a -> ([a],Zipper a)
splitAt n xs = (take n xs, drop n xs)

-- | @'takeWhile' p xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> Zipper a -> [a]
takeWhile p0 (n0 :~ f0) = go p0 n0 f0 where
  go !p !n !f
    | x <- f n, p x = x : go p (n + 1) f
    | otherwise = []

-- | @'dropWhile' p xs@ returns the suffix remaining after
-- @'takeWhile' p xs@.
--
-- /Beware/: this function may diverge if every element of @xs@
-- satisfies @p@, e.g.  @dropWhile even (repeat 0)@ will loop.
dropWhile :: (a -> Bool) -> Zipper a -> Zipper a
dropWhile p xs@(_ :~ f) = findIndex' p xs :~ f

-- | @'span' p xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
span :: (a -> Bool) -> Zipper a -> ([a], Zipper a)
span p0 (n0 :~ f0)
  | (ts, n') <- go p0 n0 f0 = (ts, n' :~ f0) where
  go !p !n !f
    | x <- f n, p x, (ts, fs) <- go p (n + 1) f = (x:ts, fs)
    | otherwise = ([], n)

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
break :: (a -> Bool) -> Zipper a -> ([a], Zipper a)
break p = span (not . p)

-- | The 'isPrefix' function returns @True@ if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> Zipper a -> Bool
isPrefixOf xs0 (n0 :~ f0) = go xs0 n0 f0 where
  go [] !_ !_ = True
  go (y:ys) n f = y == f n && go ys (n + 1) f

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
(!!) :: Zipper a -> Integer -> a
(!!) (n :~ f) m = f (n + m)

-- | The 'findIndex' function takes a predicate and a stream and returns
-- the index of the first element in the stream that satisfies the predicate,
--
-- /Beware/: 'findIndex' @p@ @xs@ will diverge if none of the elements of
-- @xs@ satisfy @p@.
findIndex :: (a -> Bool) -> Zipper a -> Integer
findIndex p0 (n0 :~ f0) = go p0 n0 f0 - n0 where
  go !p !n !f
    | x <- f n, p x = n
    | otherwise = go p (n + 1) f

-- | Internal helper, used to find an index in the
findIndex' :: (a -> Bool) -> Zipper a -> Integer
findIndex' p0 (n0 :~ f0) = go p0 n0 f0 where
  go !p !n !f
    | x <- f n, p x = n
    | otherwise = go p (n + 1) f

-- | The 'elemIndex' function returns the index of the first element
-- in the given stream which is equal (by '==') to the query element,
--
-- /Beware/: @'elemIndex' x xs@ will diverge if none of the elements
-- of @xs@ equal @x@.
elemIndex :: Eq a => a -> Zipper a -> Integer
elemIndex = findIndex . (==)

{-
-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
--
-- /Beware/: 'elemIndices' @x@ @xs@ will diverge if any suffix of
-- @xs@ does not contain @x@.
elemIndices :: Eq a => a -> Zipper a -> Zipper Int
elemIndices x = findIndices (x==)
-}

-- | The 'zip' function takes two streams and returns a list of
-- corresponding pairs.
--
-- > zip = liftA2 (,)
zip :: Zipper a -> Zipper b -> Zipper (a,b)
zip = liftA2 (,)

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the functions, the elements are combined using the function
-- passed as the first argument to 'zipWith'.
--
-- > zipWith = liftA2
zipWith :: (a -> b -> c) -> Zipper a -> Zipper b -> Zipper c
zipWith = liftA2

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Zipper (a,b) -> (Zipper a, Zipper b)
unzip xs = (fst <$> xs, snd <$> xs)



{-

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending
-- order.
--
-- /Beware/: 'findIndices' @p@ @xs@ will diverge if all the elements
-- of any suffix of @xs@ fails to satisfy @p@.
findIndices :: (a -> Bool) -> Zipper a -> Zipper Int
findIndices p = indicesFrom 0 where
  indicesFrom ix (x :< xs)
    | p x = ix :< ixs
    | otherwise = ixs
    where ixs = (indicesFrom $! (ix+1)) xs


-- | The 'words' function breaks a stream of characters into a
-- stream of words, which were delimited by white space.
--
-- /Beware/: if the stream of characters @xs@ does not contain white
-- space, accessing the tail of @words xs@ will loop.
words :: Zipper Char -> Zipper String
words xs | (w, ys) <- break isSpace xs = w :< words ys

-- | The 'unwords' function is an inverse operation to 'words'. It
-- joins words with separating spaces.
unwords :: Zipper String -> Zipper Char
unwords ~(x :< xs) = foldr (:<) (' ' :< unwords xs) x

-- | The 'lines' function breaks a stream of characters into a list
-- of strings at newline characters. The resulting strings do not
-- contain newlines.
--
-- /Beware/: if the stream of characters @xs@ does not contain
-- newline characters, accessing the tail of @lines xs@ will loop.
lines :: Zipper Char -> Zipper String
lines xs | (l, ys) <- break (== '\n') xs = l :< lines (tail ys)

-- | The 'unlines' function is an inverse operation to 'lines'. It
-- joins lines, after appending a terminating newline to each.
unlines :: Zipper String -> Zipper Char
unlines ~(x :< xs) = foldr (:<) ('\n' :< unlines xs) x

-- | The 'fromList' converts an infinite list to a
-- stream.
--
-- /Beware/: Passing a finite list, will cause an error.
fromList :: [a] -> Zipper a
fromList (x:xs) = x :< fromList xs
fromList []     = error "Zipper.listToZipper applied to finite list"

-}
