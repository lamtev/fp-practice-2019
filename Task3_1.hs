module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
-- https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1270011

instance Show WeirdPeanoNumber where
  show Zero     = "0"
  show (Succ p) = "+" ++ show p
  show (Pred p) = "-" ++ show p

fromInt :: Integer -> WeirdPeanoNumber
fromInt int | int == 0  = Zero
            | int < 0   = Pred $ fromInt (int + 1)
            | otherwise = Succ $ fromInt (int - 1)

toInt :: WeirdPeanoNumber -> Integer
toInt wpn = case wpn of
    Zero     -> 0
    Pred wpn -> toInt wpn - 1
    Succ wpn -> toInt wpn + 1

optimize :: WeirdPeanoNumber -> WeirdPeanoNumber
optimize = fromInt . toInt

instance Eq WeirdPeanoNumber where
    a == b = optimize a `eq` optimize b
      where
        Zero   `eq` Zero   = True
        Zero   `eq` _      = False
        Pred a `eq` Pred b = a == b
        Succ a `eq` Succ b = a == b
        _      `eq` _      = False

instance Ord WeirdPeanoNumber where
    compare a b = optimize a `cmp` optimize b
      where
        Zero   `cmp` Zero     = EQ
        Zero   `cmp` Succ a = LT
        Zero   `cmp` Pred a = GT
        Succ a `cmp` Succ b   = a `cmp` b
        Succ a `cmp` _        = GT
        Pred a `cmp` Pred b   = a `cmp` b
        Pred a `cmp` _        = LT

-- https://wiki.haskell.org/Num_instance_for_functions
instance Num WeirdPeanoNumber where
  negate wpn = case wpn of
    Succ a -> Pred $ negate a
    Pred a -> Succ $ negate a
    Zero -> Zero

  a + Zero   = a
  a + Succ b = Succ $ a + b
  a + Pred b = Pred $ b - a

  a * Zero   = Zero
  a * Succ b = (a + 1) * b
  a * Pred b = (a - 1) * b

  fromInteger = fromInt

  abs Zero   = 0
  abs (Succ a) = abs a + 1
  abs (Pred a) = abs a + 1

  signum wpn = case optimize wpn of
      Zero   -> Zero
      Succ _ -> Succ Zero
      Pred _ -> Pred Zero

instance Real WeirdPeanoNumber where
  toRational = toRational . toInt

instance Integral WeirdPeanoNumber where
  toInteger = toInt

  quotRem a b = (fromInt $ fst quotRemInt, fromInt $ snd quotRemInt)
    where
      quotRemInt = (quotRem (toInt a) (toInt b))

instance Enum WeirdPeanoNumber where
  toEnum   = fromInt . fromIntegral
  fromEnum = fromIntegral . toInt
  succ     = Succ
  pred     = Pred
