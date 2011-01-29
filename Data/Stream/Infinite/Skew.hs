{-# LANGUAGE PatternGuards, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Infinite.Skew
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Anticausal streams implemented as non-empty skew binary random access lists
-- 
-- The Applicative zips streams, the monad diagonalizes
------------------------------------------------------------------------------


module Data.Stream.Infinite.Skew 
    ( Stream
    , (<|)      -- O(1)
    , (!!)
    , head      -- O(1)
    , tail      -- O(1)
    , tails
    , uncons    -- O(1)
    , index     -- O(log n)
    , drop      -- O(log n)
    , dropWhile -- O(n)
    , span
    , break
    , split
    , splitW
    , repeat   
    , insert    -- O(n)
    , insertBy
    , adjust    -- O(log n)
    , update    -- O(log n)
    , fromList
    , from
    , indexed
    , interleave
    , tabulate
    ) where 

import Control.Arrow (first)
import Control.Applicative hiding (empty)
import Control.Comonad
import Data.Distributive
import Data.Functor.Alt
import Data.Functor.Apply
import Data.Foldable hiding (toList)
import Data.Traversable (Traversable, traverse)
import qualified Data.Traversable as Traversable
import Data.Semigroup hiding (Last)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Monoid (Monoid(mappend))
import Prelude hiding (null, head, tail, drop, dropWhile, length, foldr, last, span, repeat, replicate, (!!), break)

infixr 5 :<, <|

data Complete a 
    = Tip a
    | Bin !Integer a !(Complete a) !(Complete a)
    deriving Show

instance Functor Complete where
  fmap f (Tip a) = Tip (f a)
  fmap f (Bin w a l r) = Bin w (f a) (fmap f l) (fmap f r)

instance Extend Complete where
  extend f w@Tip {} = Tip (f w)
  extend f w@(Bin n _ l r) = Bin n (f w) (extend f l) (extend f r)

instance Comonad Complete where
  extract (Tip a) = a
  extract (Bin _ a _ _) = a

instance Foldable Complete where
  foldMap f (Tip a) = f a 
  foldMap f (Bin _ a l r) = f a `mappend` foldMap f l `mappend` foldMap f r
  foldr f z (Tip a) = f a z
  foldr f z (Bin _ a l r) = f a (foldr f (foldr f z r) l)

instance Foldable1 Complete where
  foldMap1 f (Tip a) = f a
  foldMap1 f (Bin _ a l r) = f a <> foldMap1 f l <> foldMap1 f r

instance Traversable Complete where
  traverse f (Tip a) = Tip <$> f a 
  traverse f (Bin n a l r) = Bin n <$> f a <*> traverse f l <*> traverse f r

instance Traversable1 Complete where
  traverse1 f (Tip a) = Tip <$> f a 
  traverse1 f (Bin n a l r) = Bin n <$> f a <.> traverse1 f l <.> traverse1 f r

bin :: a -> Complete a -> Complete a -> Complete a 
bin a l r = Bin (1 + weight l + weight r) a l r
{-# INLINE bin #-}

weight :: Complete a -> Integer
weight Tip{} = 1
weight (Bin w _ _ _) = w
{-# INLINE weight #-}

-- A future is a non-empty skew binary random access list of nodes.
-- The last node, however, is allowed to contain fewer values. 
data Stream a = !(Complete a) :< Stream a
--  deriving Show

instance Show a => Show (Stream a) where
  showsPrec d as = showParen (d >= 10) $ 
    showString "fromList " . showsPrec 11 (toList as)

instance Functor Stream where
  fmap f (t :< ts) = fmap f t :< fmap f ts

instance Extend Stream where
  extend g0 (t :< ts) = go g0 t (:< ts) :< extend g0 ts
    where 
      go :: (Stream a -> b) -> Complete a -> (Complete a -> Stream a) -> Complete b
      go g w@Tip{}         f = Tip (g (f w))
      go g w@(Bin n _ l r) f = Bin n (g (f w)) (go g l (:< f r))  (go g r f)

instance Comonad Stream where
  extract = head

instance Apply Stream where
  fs <.> as = mapWithIndex (\n f -> f (as !! n)) fs
  as <.  _  = as
  _   .> bs = bs

instance Applicative Stream where
  pure = repeat
  (<*>) = (<.>)
  (<* ) = (<. )
  ( *>) = ( .>)

instance Alt Stream where
  as <!> bs = tabulate $ \i -> case quotRem i 2 of 
    (q,0) -> as !! q
    (q,_) -> bs !! q


instance Foldable Stream where
  foldMap f (t :< ts) = foldMap f t `mappend` foldMap f ts
  foldr f z (t :< ts) = foldr f (foldr f z ts) t

toList :: Stream a -> [a]
toList = foldr (:) []

instance Foldable1 Stream where
  foldMap1 f (t :< ts) = foldMap1 f t <> foldMap1 f ts

instance Traversable Stream where
  traverse f (t :< ts) = (:<) <$> traverse f t <*> traverse f ts

instance Traversable1 Stream where
  traverse1 f (t :< ts) = (:<) <$> traverse1 f t <.> traverse1 f ts

instance Distributive Stream where
  distribute w = tabulate (\i -> fmap (!! i) w)

instance Semigroup (Stream a) where
  (<>) = (<!>)

instance Monad Stream where
  return = pure
  as >>= f = mapWithIndex (\i a -> f a !! i) as

interleave :: Stream a -> Stream a -> Stream a
interleave = (<!>) 
      
repeat :: a -> Stream a 
repeat b = go b (Tip b) 
    where 
      go :: a -> Complete a -> Stream a 
      go a as | ass <- bin a as as = as :< go a ass

mapWithIndex :: (Integer -> a -> b) -> Stream a -> Stream b
mapWithIndex f0 as0 = spine f0 0 as0
  where 
    spine f m (a :< as) = tree f m a :< spine f (m + weight a) as
    tree f m (Tip a) = Tip (f m a)
    tree f m (Bin n a l r) = Bin n (f m a) (tree f (m + 1) l) (tree f (m + 1 + weight l) r)

tabulate :: (Integer -> a) -> Stream a
tabulate f = mapWithIndex (const . f) (pure ())


indexed :: Stream a -> Stream (Integer, a)
indexed = mapWithIndex (,)

from :: Num a => a -> Stream a
from a = mapWithIndex ((+) . fromIntegral) (pure a)

-- | /O(1)/ cons
(<|) :: a -> Stream a -> Stream a
a <| (l :< r :< as) 
  | weight l == weight r = bin a l r :< as
a <| as = Tip a :< as
{-# INLINE (<|) #-}

-- | /O(1)/
head :: Stream a -> a
head (a :< _) = extract a
{-# INLINE head #-}

-- | /O(1)/.
tail :: Stream a -> Stream a
tail (Tip{} :< ts) = ts
tail (Bin _ _ l r :< ts) = l :< r :< ts
{-# INLINE tail #-}

tails :: Stream a -> Stream (Stream a)
tails = duplicate
{-# INLINE tails #-}

-- | /O(1)/.
uncons :: Stream a -> (a, Stream a)
uncons (Tip a       :< as)  = (a, as)
uncons (Bin _ a l r :< as)  = (a, l :< r :< as)
{-# INLINE uncons #-}

-- | /O(log n)/.
index :: Integer -> Stream a -> a
index i (t :< ts) 
  | i < 0     = error "index: negative index"
  | i < w     = indexComplete i t
  | otherwise = index (i - w) ts
  where w = weight t

indexComplete :: Integer -> Complete a -> a
indexComplete 0 (Tip a) = a
indexComplete 0 (Bin _ a _ _) = a
indexComplete i (Bin w _ l r) 
  | i <= w'   = indexComplete (i-1) l
  | otherwise = indexComplete (i-1-w') r
  where w' = div w 2
indexComplete _ _ = error "indexComplete"

-- | /O(log n)/.
(!!) :: Stream a -> Integer -> a
(!!) = flip index 

-- | /O(log n)/.
drop :: Integer -> Stream a -> Stream a
drop 0 ts = ts
drop i (t :< ts) = case compare i w of
  LT -> dropComplete i t (:< ts)
  EQ -> ts
  GT -> drop (i - w) ts
  where w = weight t

dropComplete :: Integer -> Complete a -> (Complete a -> Stream a) -> Stream a 
dropComplete 0 t f             = f t
dropComplete 1 (Bin _ _ l r) f = l :< f r
dropComplete i (Bin w _ l r) f = case compare (i - 1) w' of
    LT -> dropComplete (i-1) l (:< f r)
    EQ -> f r
    GT -> dropComplete (i-1-w') r f
    where w' = div w 2
dropComplete _ _ _ = error "dropComplete"

-- /O(n)/.
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p as 
  | p (head as) = dropWhile p (tail as)
  | otherwise   = as

-- /O(n)/
span :: (a -> Bool) -> Stream a -> ([a], Stream a)
span p as
  | a <- head as, p a = first (a:) $ span p (tail as)
  | otherwise = ([], as)

-- /O(n)/
break :: (a -> Bool) -> Stream a -> ([a], Stream a)
break p = span (not . p)

-- /(O(n), O(log n))/ split at _some_ edge where function goes from False to True.
-- best used with a monotonic function
split :: (a -> Bool) -> Stream a -> ([a], Stream a)
split p (a :< as)
  | p (extract as) = splitComplete p a (:< as)
  | (ts, fs) <- split p as = (foldr (:) ts a, fs)

-- for use when we know the split occurs within a given tree
splitComplete :: (a -> Bool) -> Complete a -> (Complete a -> Stream a) -> ([a], Stream a)
splitComplete _ t@Tip{} f = ([], f t)
splitComplete p t@(Bin _ a l r) f
  | p a                                                   = ([], f t)
  | p (extract r), (ts, fs) <- splitComplete p l (:< f r) = (a:ts, fs)
  |                (ts, fs) <- splitComplete p r f        = (a:foldr (:) ts l, fs)

-- /(O(n), O(log n))/ split at _some_ edge where function goes from False to True.
-- best used with a monotonic function
--
-- > splitW p xs = (map extract &&& fmap (fmap extract)) . split p . duplicate
splitW :: (Stream a -> Bool) -> Stream a -> ([a], Stream a)
splitW p (a :< as) 
  | p as                    = splitCompleteW p a (:< as)
  | (ts, fs) <- splitW p as = (foldr (:) ts a, fs)

-- for use when we know the split occurs within a given tree
splitCompleteW :: (Stream a -> Bool) -> Complete a -> (Complete a -> Stream a) -> ([a], Stream a)
splitCompleteW _ t@Tip{} f = ([], f t)
splitCompleteW p t@(Bin _ a l r) f
  | w <- f t, p w                                        = ([], w)
  | w <- f r, p w, (ts, fs) <- splitCompleteW p l (:< w) = (a:ts, fs)
  |                (ts, fs) <- splitCompleteW p r f      = (a:foldr (:) ts l, fs)

fromList :: [a] -> Stream a
fromList = foldr (<|) (error "fromList: finite list")

-- /O(n)/
insert :: Ord a => a -> Stream a -> Stream a
insert a as | (ts, as') <- split (a<=) as = foldr (<|) (a <| as') ts

-- /O(n)/. Finds the split in O(log n), but then has to recons
insertBy :: (a -> a -> Ordering) -> a -> Stream a -> Stream a
insertBy cmp a as | (ts, as') <- split (\b -> cmp a b <= EQ) as = foldr (<|) (a <| as') ts

-- /O(log n)/ Change the value of the nth entry in the future
adjust :: Integer -> (a -> a) -> Stream a -> Stream a
adjust !n f (a :< as) 
  | n < w = adjustComplete n f a :< as
  | otherwise = a :< adjust (n - w) f as
  where w = weight a

adjustComplete :: Integer -> (a -> a) -> Complete a -> Complete a
adjustComplete 0 f (Tip a) = Tip (f a)
adjustComplete _ _ t@Tip{} = t
adjustComplete n f (Bin m a l r) 
  | n == 0 = Bin m (f a) l r
  | n < w = Bin m a (adjustComplete (n - 1) f l) r
  | otherwise = Bin m a l (adjustComplete (n - 1 - w) f r)
  where w = weight l

update :: Integer -> a -> Stream a -> Stream a
update n = adjust n . const

