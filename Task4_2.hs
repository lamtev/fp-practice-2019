module Task4_2 where

import Prelude hiding (fst, snd)

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf { fst :: a,
                         snd :: a,
                         trd :: a,
                         fth :: a } deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
  fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
  pure x = FourOf x x x x

  -- (<*>) :: (FourOf (a -> b)) -> (FourOf a) -> (FourOf b)
  (<*>) (FourOf f g h q) (FourOf a b c d) = FourOf (f a) (g b) (h c) (q d)

instance Monad FourOf where
  -- (>>=) :: (FourOf a) -> (a -> (FourOf b)) -> (FourOf b)
  (FourOf a b c d) >>= kl = FourOf (fst (kl a)) (snd (kl b)) (trd (kl c)) (fth (kl d))

actual = do
          x <- FourOf 1 2 3 4
          y <- FourOf 4 6 7 8
          return $ x + y
expected = FourOf 5 8 10 12

test = if actual == expected then True else error "assertion failed"
