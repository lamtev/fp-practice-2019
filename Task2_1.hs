module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Node{ key::Integer, value::v, lch::TreeMap v, rch::TreeMap v }
               deriving Show

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _                   = False
contains (Node tk v l r) k | tk == k   = True
                           | k < tk    = contains l k
                           | otherwise = contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> Maybe v
lookup _ EmptyTree                   = Nothing
lookup k (Node tk v l r) | k == tk   = Just v
                         | k < tk    = lookup k l
                         | otherwise = lookup k r

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTree                    = Node k v EmptyTree EmptyTree
insert (k, v) (Node tk tv l r) | k == tk   = Node k v l r
                               | k < tk    = Node k v (insert (k, v) l) r
                               | otherwise = Node k v l (insert (k, v) r)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTree = EmptyTree
remove k (Node tk v l r) | k == tk   = helper l r
                         | k < tk    = Node tk v (remove k l) r
                         | otherwise = Node tk v l (remove k r)
                            where helper l r = case (l, r) of
                                                  (EmptyTree, EmptyTree) -> EmptyTree
                                                  (EmptyTree, _)         -> r
                                                  (_, EmptyTree)         -> l
                                                  (_, _)                 -> Node (key l) (value l) (lch l) (helper (rch l) r)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Node k v l r) = listFromTree l ++ [(k, v)] ++ listFromTree r

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
