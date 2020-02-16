module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}


import Data.List (sort)
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
                               | k < tk    = Node tk tv (insert (k, v) l) r
                               | otherwise = Node tk tv l (insert (k, v) r)

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
nearestLE :: Integer -> TreeMap v -> Maybe (Integer, v)
nearestLE _ EmptyTree = Nothing
nearestLE k (Node tk v l r) | k == tk = Just (k, v)
                            | k < tk  = nearestLE k l
                            | k > tk  = less
                                          where less = case nearestLE k r of
                                                              Nothing -> Just (tk, v)
                                                              it@_    -> it

-- Тест
list = [(1, 0), (30, 0), (2, 0), (50, 0), (4, 0), (70, 0), (6, 0), (8, 0)]
tree = treeFromList list
nearestLETest1 = if nearestLE (-1) tree /= Nothing then error "assertion failed" else True
nearestLETest2 = if nearestLE 69 tree /= Just (50, 0) then error "assertion failed" else True

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Node k v l r) = listFromTree l ++ [(k, v)] ++ listFromTree r

-- Тест
listFromTreeAndTreeFromListTest = if (sort list) /= (listFromTree (treeFromList list)) then error "assertion failed" else True

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> Maybe (Integer, v)
kMean i EmptyTree = Nothing
kMean i (Node k v l r) | lCount == i = Just (k, v)
                       | lCount > i  = kMean i l
                       | otherwise   = kMean (i - lCount - 1) r
                          where lCount = count l
                                count tree = case tree of
                                         EmptyTree    -> 0
                                         Node _ _ l r -> 1 + count l + count r

-- Тест
kMeanTest = if Just (listFromTree tree !! 5) /= kMean 5 tree then error "assertion failed" else True
