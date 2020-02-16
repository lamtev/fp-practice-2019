module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}


import Data.Maybe (fromJust, isJust)
import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

-- Сначала происходит раскрутка всего списка, а затем выполняется вычисление. Это не очень эффективно
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Сначала вычисляем значение функции
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = apply `seq` foldl' f apply xs
                      where apply = f z x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f z = case f z of
   Nothing -> []
   Just (a, b) -> a : unfoldr f b

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse l = foldl (\xs x -> x:xs) [] l

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr helper [] where helper x acc = f x : acc

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
-- На заметил, что надо на основе foldr и unfoldr все реализовать :)
catMaybes :: [Maybe a] -> [a]
catMaybes l = [fromJust e | e <- l, isJust e]

catMaybes' :: [Maybe a] -> [a]
catMaybes' l = foldr appendIfJust [] l
  where
    appendIfJust Nothing xs = xs
    appendIfJust (Just x) xs = x : xs

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal m = unfoldr (\m -> if null m then Nothing else Just (head (head m), tail (map tail m))) m

-- Тест
m = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
diagonalTest = if diagonal m /= [1, 1, 1] then error "assertion failed" else True

-- Фильтр для всех элементов, не соответствующих предикату
-- На заметил, что надо на основе foldr и unfoldr все реализовать :)
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pr l = [e | e <- l, not $ pr e]

filterNot' :: (a -> Bool) -> [a] -> [a]
filterNot' pr l = foldr (\x xs -> if not $ pr x then x : xs else xs) [] l

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem y = foldl f False where f b x = if b || x == y then True else False

-- Список чисел в диапазоне [from, to) с шагом step
-- На заметил, что надо на основе foldr и unfoldr все реализовать :)
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = [e | e <- [from..to-1], (e - from) `mod` step == 0]

rangeTo' :: Integer -> Integer -> Integer -> [Integer]
rangeTo' from to step = unfoldr (\e -> if e < to && step > 0 || e > to && step < 0 then Just (e, e + step) else Nothing) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append = foldr (:)

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups l n = let intN = fromIntegral n in
                unfoldr (\l -> if null l then Nothing else Just (take intN l, drop intN l)) l
