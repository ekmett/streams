{-# LANGUAGE PatternGuards, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Stream.Future.Skew
-- Copyright   :  (C) 2008-2011 Edward Kmett,
--                (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Anticausal streams implemented as non-empty skew binary random access lists
-- 
-- The Applicative zips streams, but since these are potentially infinite
-- this is stricter than would be desired. You almost always want 
------------------------------------------------------------------------------


module Data.Stream.Future.Skew 
    ( Future(..)
    , (<|)      -- O(1)
    , cons
    , length    -- O(log n)
    , head      -- O(1)
    , tail      -- O(1)
    , tails
    , last      -- O(log n)
    , uncons    -- O(1)
    , index     -- O(log n)
    , drop      -- O(log n)
    , dropWhile -- O(n)
    , indexed
    , from
    , break
    , span 
    , split     -- O(log n)
    , splitW    -- O(log n)
    , repeat   
    , replicate -- O(log n) 
    , insert    -- O(n)
    , insertBy
    , update
    , adjust    -- O(log n)
    , fromList
    , toFuture
    ) where 

import Control.Applicative hiding (empty)
import Control.Comonad
import Control.Comonad.Apply
import Data.Functor.Alt
import Data.Functor.Apply
import Data.Foldable hiding (toList)
import Data.Traversable (Traversable, traverse)
import qualified Data.Traversable as Traversable
import Data.Semigroup hiding (Last)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Monoid (Monoid(mappend))
import Prelude hiding (null, head, tail, drop, dropWhile, length, foldr, last, span, repeat, replicate, break)

infixr 5 :<, <|

data Complete a 
    = Tip a
    | Bin {-# UNPACK #-} !Int a !(Complete a) !(Complete a)
    deriving Show

instance Functor Complete where
  fmap f (Tip a) = Tip (f a)
  fmap f (Bin w a l r) = Bin w (f a) (fmap f l) (fmap f r)

instance Comonad Complete where
  extract (Tip a) = a
  extract (Bin _ a _ _) = a
  extend f w@Tip {} = Tip (f w)
  extend f w@(Bin n _ l r) = Bin n (f w) (extend f l) (extend f r)

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

weight :: Complete a -> Int
weight Tip{} = 1
weight (Bin w _ _ _) = w
{-# INLINE weight #-}

-- A future is a non-empty skew binary random access list of nodes.
-- The last node, however, is allowed to contain fewer values. 
data Future a 
  = Last !(Complete a) 
  | !(Complete a) :< Future a
--  deriving Show


instance Show a => Show (Future a) where
  showsPrec d as = showParen (d >= 10) $ 
    showString "fromList " . showsPrec 11 (toList as)

instance Functor Future where
  fmap f (t :< ts) = fmap f t :< fmap f ts
  fmap f (Last t) = Last (fmap f t)

instance Comonad Future where
  extract = head
  extend g (Last t)  = Last (extendTree g t Last)
  extend g (t :< ts) = extendTree g t (:< ts) :< extend g ts

extendTree :: (Future a -> b) -> Complete a -> (Complete a -> Future a) -> Complete b
extendTree g w@Tip{}         f = Tip (g (f w))
extendTree g w@(Bin n _ l r) f = Bin n (g (f w)) (extendTree g l (:< f r))  (extendTree g r f)

instance FunctorApply Future where
  Last (Tip f)         <.> as                   = singleton (f (extract as))
  fs                   <.> Last (Tip a)         = singleton (extract fs a)
  Last (Bin _ f lf rf) <.> Last (Bin _ a la ra) = f a <| (lf :< Last rf  <.> la :< Last ra )
  Last (Bin _ f lf rf) <.> Bin _ a la ra :< as  = f a <| (lf :< Last rf  <.> la :< ra :< as)
  Last (Bin _ f lf rf) <.> Tip a :< as          = f a <| (lf :< Last rf  <.> as            )
  Bin _ f lf rf :< fs  <.> Last (Bin _ a la ra) = f a <| (lf :< rf :< fs <.> la :< Last ra )
  Bin _ f lf rf :< fs  <.> Tip a :< as          = f a <| (lf :< rf :< fs <.> as            )
  Bin _ f lf rf :< fs  <.> Bin _ a la ra :< as  = f a <| (lf :< rf :< fs <.> la :< ra :< as)
  Tip f :< fs          <.> Tip a :< as          = f a <| (fs             <.> as            )
  Tip f :< fs          <.> Bin _ a la ra :< as  = f a <| (fs             <.> la :< ra :< as)
  Tip f :< fs          <.> Last (Bin _ a la ra) = f a <| (fs             <.> la :< Last ra )

instance ComonadApply Future

instance Applicative Future where
  pure = repeat
  (<*>) = (<.>)

instance FunctorAlt Future where
  as <!> bs = foldr (<|) bs as

instance Foldable Future where
  foldMap f (t :< ts) = foldMap f t `mappend` foldMap f ts
  foldMap f (Last t) = foldMap f t
  foldr f z (t :< ts) = foldr f (foldr f z ts) t
  foldr f z (Last t) = foldr f z t

toList :: Future a -> [a]
toList = foldr (:) []

instance Foldable1 Future where
  foldMap1 f (t :< ts) = foldMap1 f t <> foldMap1 f ts
  foldMap1 f (Last t) = foldMap1 f t

instance Traversable Future where
  traverse f (t :< ts) = (:<) <$> traverse f t <*> traverse f ts
  traverse f (Last t) = Last <$> traverse f t

instance Traversable1 Future where
  traverse1 f (t :< ts) = (:<) <$> traverse1 f t <.> traverse1 f ts
  traverse1 f (Last t) = Last <$> traverse1 f t

repeat :: a -> Future a 
repeat a0 = go a0 (Tip a0) 
    where 
      go :: a -> Complete a -> Future a 
      go a as | ass <- bin a as as = as :< go a ass
{-# INLINE repeat #-}

-- | /O(log n)/
replicate :: Int -> a -> Future a
replicate n a
  | n <= 0    = error "replicate: non-positive argument"
  | otherwise = go 1 n a (Tip a) (\0 r -> r)
  where 
  -- invariants: 
  -- tb is a complete tree of i nodes all equal to b
  -- 1 <= i = 2^m-1 <= j
  -- k accepts r such that 0 <= r < i
  go :: Int -> Int -> b -> Complete b -> (Int -> Future b -> r) -> r
  go !i !j b tb k 
    | j >= i2p1 = go i2p1 j b (Bin i2p1 b tb tb) k'
    | j >= i2   = k (j - i2) (tb :< Last tb)
    | otherwise = k (j - i) (Last tb)
    where 
      i2 = i * 2
      i2p1 = i2 + 1
      k' r xs 
        | r >= i2   = k (r - i2) (tb :< tb :< xs)
        | r >= i    = k (r - i) (tb :< xs)
        | otherwise = k r xs
{-# INLINE replicate #-}

mapWithIndex :: (Int -> a -> b) -> Future a -> Future b
mapWithIndex f0 as0 = spine f0 0 as0
  where 
    spine f m (Last as) = Last (tree f m as)
    spine f m (a :< as) = tree f m a :< spine f (m + weight a) as
    tree f m (Tip a) = Tip (f m a)
    tree f m (Bin n a l r) = Bin n (f m a) (tree f (m + 1) l) (tree f (m + 1 + weight l) r)

indexed :: Future a -> Future (Int, a)
indexed = mapWithIndex (,)
{-# INLINE indexed #-}

from :: Num a => a -> Future a
from a = mapWithIndex ((+) . fromIntegral) (pure a)
{-# INLINE from #-}

-- | /O(1)/
singleton :: a -> Future a 
singleton a = Last (Tip a)
{-# INLINE singleton #-}

-- | /O(log n)/.
length :: Future a -> Int
length (Last t) = weight t
length (t :< ts) = weight t + length ts

-- | /O(1)/ cons
(<|) :: a -> Future a -> Future a
a <| (l :< Last r)  
  | weight l == weight r = Last (bin a l r)
a <| (l :< r :< as) 
  | weight l == weight r = bin a l r :< as
a <| as = Tip a :< as
{-# INLINE (<|) #-}


cons :: a -> Future a -> Future a
cons = (<|)  
{-# INLINE cons #-}

-- | /O(1)/
head :: Future a -> a
head (a :< _) = extract a
head (Last a) = extract a 
{-# INLINE head #-}

-- | /O(1)/.
tail :: Future a -> Maybe (Future a)
tail (Tip{} :< ts) = Just ts
tail (Bin _ _ l r :< ts) = Just (l :< r :< ts)
tail (Last Tip{}) = Nothing
tail (Last (Bin _ _ l r)) = Just (l :< Last r)
{-# INLINE tail #-}

tails :: Future a -> Future (Future a)
tails = duplicate
{-# INLINE tails #-}

-- | /O(log n)/.
last :: Future a -> a
last (_ :< as) = last as
last (Last as) = go as
  where go (Tip a) = a
        go (Bin _ _ _ r) = go r

-- | /O(1)/.
uncons :: Future a -> (a, Maybe (Future a))
uncons (Last (Tip a))       = (a, Nothing)
uncons (Last (Bin _ a l r)) = (a, Just (l :< Last r))
uncons (Tip a       :< as)  = (a, Just as)
uncons (Bin _ a l r :< as)  = (a, Just (l :< r :< as))
{-# INLINE uncons #-}

-- | /O(log n)/.
index :: Int -> Future a -> a
index i (Last t) 
  | i < weight t = indexComplete i t
  | otherwise    = error "index: out of range"
index i (t :< ts) 
  | i < w     = indexComplete i t
  | otherwise = index (i - w) ts
  where w = weight t

indexComplete :: Int -> Complete a -> a
indexComplete 0 (Tip a) = a
indexComplete i (Bin w a l r) 
  | i == 0    = a
  | i <= w'   = indexComplete (i-1) l
  | otherwise = indexComplete (i-1-w') r
  where w' = div w 2
indexComplete _ _ = error "index: index out of range"

-- | /O(log n)/.
drop :: Int -> Future a -> Maybe (Future a)
drop 0 ts = Just ts
drop i (t :< ts) = case compare i w of
  LT -> Just (dropComplete i t (:< ts))
  EQ -> Just ts
  GT -> drop (i - w) ts
  where w = weight t
drop i (Last t) 
  | i < w     = Just (dropComplete i t Last)
  | otherwise = Nothing
  where w = weight t

dropComplete :: Int -> Complete a -> (Complete a -> Future a) -> Future a 
dropComplete 0 t f             = f t
dropComplete 1 (Bin _ _ l r) f = l :< f r
dropComplete i (Bin w _ l r) f = case compare (i - 1) w' of
  LT -> dropComplete (i-1) l (:< f r)
  EQ -> f r
  GT -> dropComplete (i-1-w') r f 
  where w' = div w 2
dropComplete _ _ _ = error "drop: index out of range"

-- /O(n)/.
dropWhile :: (a -> Bool) -> Future a -> Maybe (Future a)
dropWhile p as 
  | p (head as) = tail as >>= dropWhile p
  | otherwise = Just as

-- /O(n)/
span :: (a -> Bool) -> Future a -> ([a], Maybe (Future a))
span p aas = case uncons aas of
  (a, Just as) | p a, (ts, fs) <- span p as -> (a:ts, fs)
  (a, Nothing) | p a                        -> ([a], Nothing)
  (_, _)                                    -> ([], Just aas)

-- /O(n)/
break :: (a -> Bool) -> Future a -> ([a], Maybe (Future a))
break p = span (not . p)

-- /(O(n), O(log n))/ split at _some_ edge where function goes from False to True.
-- best used with a monotonic function
split :: (a -> Bool) -> Future a -> ([a], Maybe (Future a))
split p l@(Last a) 
  | p (extract a)  = ([], Just l)
  | otherwise      = splitComplete p a Last
split p (a :< as)
  | p (extract as) = splitComplete p a (:< as)
  | (ts, fs) <- split p as = (foldr (:) ts a, fs)

-- for use when we know the split occurs within a given tree
splitComplete :: (a -> Bool) -> Complete a -> (Complete a -> Future a) -> ([a], Maybe (Future a))
splitComplete p t@(Tip a) f
  | p a       = ([], Just (f t))
  | otherwise = ([a], Nothing)
splitComplete p t@(Bin _ a l r) f
  | p a                                               = ([], Just (f t))
  | p (extract r), (ts, fs) <- splitComplete p l (:< f r) = (a:ts, fs)
  |                (ts, fs) <- splitComplete p r f        = (a:foldr (:) ts l, fs)

-- /(O(n), O(log n))/ split at _some_ edge where function goes from False to True.
-- best used with a monotonic function
--
-- > splitW p xs = (map extract &&& fmap (fmap extract)) . split p . duplicate
splitW :: (Future a -> Bool) -> Future a -> ([a], Maybe (Future a))
splitW p l@(Last a)
  | p l       = ([], Just l)
  | otherwise = splitCompleteW p a Last
splitW p (a :< as) 
  | p as                    = splitCompleteW p a (:< as)
  | (ts, fs) <- splitW p as = (foldr (:) ts a, fs)

-- for use when we know the split occurs within a given tree
splitCompleteW :: (Future a -> Bool) -> Complete a -> (Complete a -> Future a) -> ([a], Maybe (Future a))
splitCompleteW p t@(Tip a) f
  | w <- f t, p w = ([], Just w)
  | otherwise = ([a], Nothing)
splitCompleteW p t@(Bin _ a l r) f
  | w <- f t, p w                                    = ([], Just w)
  | w <- f r, p w, (ts, fs) <- splitCompleteW p l (:< w) = (a:ts, fs)
  |                (ts, fs) <- splitCompleteW p r f      = (a:foldr (:) ts l, fs)

fromList :: [a] -> Future a
fromList [] = error "fromList: empty list"
fromList (x:xs) = go x xs
  where go a [] = singleton a
        go a (b:bs) = a <| go b bs

toFuture :: [a] -> Maybe (Future a) 
toFuture [] = Nothing
toFuture xs = Just (fromList xs)

-- /O(n)/
insert :: Ord a => a -> Future a -> Future a
insert a as = case split (a<=) as of
    (_, Nothing)  -> foldr (<|) (singleton a) as
    (ts, Just as') -> foldr (<|) (a <| as') ts

-- /O(n)/. Finds the split in O(log n), but then has to recons
insertBy :: (a -> a -> Ordering) -> a -> Future a -> Future a
insertBy cmp a as = case split (\b -> cmp a b <= EQ) as of
    (_, Nothing)  -> foldr (<|) (singleton a) as
    (ts, Just as') -> foldr (<|) (a <| as') ts

-- /O(log n)/ Change the value of the nth entry in the future
adjust :: Int -> (a -> a) -> Future a -> Future a
adjust !n f d@(Last a) 
  | n < weight a = Last (adjustComplete n f a)
  | otherwise = d
adjust !n f (a :< as) 
  | n < w = adjustComplete n f a :< as
  | otherwise = a :< adjust (n - w) f as
  where w = weight a

adjustComplete :: Int -> (a -> a) -> Complete a -> Complete a
adjustComplete 0 f (Tip a) = Tip (f a)
adjustComplete _ _ t@Tip{} = t
adjustComplete n f (Bin m a l r) 
  | n == 0 = Bin m (f a) l r
  | n < w = Bin m a (adjustComplete (n - 1) f l) r
  | otherwise = Bin m a l (adjustComplete (n - 1 - w) f r)
  where w = weight l

update :: Int -> a -> Future a -> Future a
update n = adjust n . const
