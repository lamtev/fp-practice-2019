module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
  fmap ff (FunMonad fmf) = FunMonad (ff . fmf)

-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html
instance Applicative FunMonad where
  -- Вкладываем значение в контекст
  pure x = FunMonad (\_ -> x)

  -- Последовательное применение, похоже на оператор ($), но в отличие от ($),
  -- (<*>) работает не с "сырыми" значениями, а со значениями, упакованными в контейнер.
  -- (<*>) :: (FunMonad ((String -> a) -> b)) -> (FunMonad (String -> a)) -> (FunMonad (String -> b))
  (<*>) (FunMonad fm1) (FunMonad fm2) = FunMonad (\s -> fm1 s (fm2 s))

-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
instance Monad FunMonad where
  -- return уже есть от Applicative pure

  -- Связываем 2 монодических вычисления: из первой монады вытаскиваем значение (a),
  -- затем применяем к этому значению стрелку Клэйсли, получаем монаду с функцией, возвращающей b.
  -- Применяем fun к монаде и s в рамках результирующей лямбды с аргументом s.
  -- (>>=) :: (FunMonad (String -> a)) -> (a -> (FunMonad (String -> b))) -> (FunMonad (String -> b))
  (FunMonad fm) >>= kl = FunMonad (\s -> fun (kl (fm s)) s)
