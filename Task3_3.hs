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

-- Monoid по вычитанию множеств
-- Вычитание - некоммутативная операция => моноид никак не построить


-- Functor
-- TODO:
