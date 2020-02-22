module Task3_3 where

{-
  Задание 3.3
  Множество на основе предикатов
-}

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Monoid по сложению (объединению) множеств. Нуль = False
newtype PSetUnion a = PSetUnion{ ucontains :: (a -> Bool) }

instance Semigroup (PSetUnion a) where
  sg1 <> sg2 = PSetUnion (\x -> ucontains sg1 x || ucontains sg2 x)

instance Monoid (PSetUnion a) where
  mempty = PSetUnion (\_ -> False)

-- Monoid по умножению (пересечению) множеств. Нуль = True
newtype PSetIntersection a = PSetIntersection{ icontains :: (a -> Bool) }

instance Semigroup (PSetIntersection a) where
  sg1 <> sg2 = PSetIntersection (\x -> icontains sg1 x && icontains sg2 x)

instance Monoid (PSetIntersection a) where
  mempty = PSetIntersection (\_ -> True)

-- Monoid по исключающему-или (логическому вычитанию) множеств
newtype PSetLogicalDifference a = PSetLogicalDifference{ ldcontains :: (a -> Bool) }

instance Semigroup (PSetLogicalDifference a) where
  sg1 <> sg2 = PSetLogicalDifference (\x -> ldcontains sg1 x /= ldcontains sg2 x)

instance Monoid (PSetLogicalDifference a) where
  mempty = PSetLogicalDifference (\_ -> False)

inFirstRange  x = x `elem` [1..5]
inSecondRange x = x `elem` [3..7]
fullRange = [1..7]

set1 = (PSetUnion             inFirstRange) `mappend` (PSetUnion             inSecondRange)
set2 = (PSetIntersection      inFirstRange) `mappend` (PSetIntersection      inSecondRange)
set3 = (PSetLogicalDifference inFirstRange) `mappend` (PSetLogicalDifference inSecondRange)

unionTest = if [e | e <- fullRange, set1 `ucontains` e] /= fullRange then error "assertion failed" else True

intersectionTest = if [e | e <- fullRange, set2 `icontains` e] /= [3..5] then error "assertion failed" else True

logicalDifferenceTest = if [e | e <- fullRange, set3 `ldcontains` e] /= [1,2,6,7] then error "assertion failed" else True

-- Monoid по вычитанию множеств
-- Вычитание - некоммутативная операция => моноид никак не построить


-- Functor

-- По условию задачи множество задается функцией, которая говорит, содержит ли множество заданный элемент. (contains)
-- Эта функция возвращает Bool
-- Тип значений, которые могут находиться во множестве, определяется типом аргумента функции contains.
-- Чтобы реализовать функтор надо как-то умудриться с помощью mapping-функции (a -> b)
-- заменить определение функции contains: (a -> Bool) -> (b -> Bool)
-- А это сделать нельзя
