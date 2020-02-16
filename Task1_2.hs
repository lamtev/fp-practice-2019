module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)
import Prelude hiding(gcd)

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd 0 0 = error("x == y == 0")
gcd x 0 = x
gcd 0 y = y
gcd x y = last [n | n <- [1..min x y], x `mod` n == 0, y `mod` n == 0]

gcd' :: Integer -> Integer -> Integer
gcd' x 0 = x
gcd' x y = gcd y $ x `mod` y

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = not $ null [n | n <- [from..to - 1], (fromIntegral $ round $ sqrt $ fromIntegral n) == (sqrt $ fromIntegral n)]

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow _ 0 = 1
pow x y = x * pow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x < 2 = False
isPrime x         = null [n | n <- [2..round $ sqrt $ fromIntegral x], x `mod` n == 0]

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
-- будем считать отрицательный год до н.э.
isDateCorrect day month _    | day <= 0 || month <= 0             = False
isDateCorrect day month year | elem month [1, 3, 5, 7, 8, 10, 12] = day <= 31
                             | month == 2                         = if isLeap year then day <= 29 else day <= 28
                             | otherwise                          = day <= 30
                                 where isLeap y = y `mod` 400 == 0 || y `mod` 4 == 0 && y `mod` 100 /= 0


-- TODO отложим до лучших времен :(

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
