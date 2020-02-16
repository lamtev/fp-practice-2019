module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)
import Prelude hiding (tail, head)

data ReverseList a = RNil | RCons (ReverseList a) a
  deriving Show

rlistToList :: ReverseList a -> [a]
rlistToList RNil         = []
rlistToList (RCons xs x) = rlistToList xs ++ [x]

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList l  = listToRListWithoutReversing $ reverse l
                   where
                     listToRListWithoutReversing []     = RNil
                     listToRListWithoutReversing (x:xs) = RCons (listToRListWithoutReversing xs) x

head :: ReverseList a -> ReverseList a
head (RCons xs x) = xs
head RNil         = error "ReverseList empty"

tail :: ReverseList a -> a
tail (RCons xs x) = x
tail RNil         = error "ReverseList empty"

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance Eq a => Eq (ReverseList a) where
  RNil         == RNil         = True
  RCons rl1 t1 == RCons rl2 t2 = rl1 == rl2 && t1 == t2
  _            == _            = False

instance (Ord a) => Ord (ReverseList a) where
  RNil       <= _          = True
  _          <= RNil       = False
  RCons xs x <= RCons ys y = x <= y && xs <= ys

instance Monoid (ReverseList a) where
  mempty = RNil

  mappend a RNil          = a
  mappend RNil a          = a
  mappend xs (RCons ys y) = RCons (mappend xs ys) y

instance Semigroup (ReverseList a) where
  xs <> ys = foldl RCons xs ys

-- https://wiki.haskell.org/Foldable_and_Traversable
instance Foldable ReverseList where
  foldMap f RNil         = mempty
  foldMap f (RCons xs x) = (foldMap f xs) <> (f x)

  foldr f z RNil         = z
  foldr f z (RCons xs x) = foldr f (f x z) xs

-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor.html
instance Functor ReverseList where
  fmap f RNil         = RNil
  fmap f (RCons xs x) = RCons (fmap f xs) (f x)
