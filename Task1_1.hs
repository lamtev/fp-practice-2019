module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

data Op = Add | Sub | Mul deriving (Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, op :: Op, rhv :: Term } -- бинарная операция
            deriving(Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
infixl 6 |+|
(|+|) l r = BinaryTerm l Add r

(|-|) :: Term -> Term -> Term
infixl 6 |-|
(|-|) l r = BinaryTerm l Sub r

(|*|) :: Term -> Term -> Term
infixl 7 |*|
(|*|) l r = BinaryTerm l Mul r

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant c)     = IntConstant c
replaceVar varName replacement (Variable v)        = if v == varName then replacement else Variable v
replaceVar varName replacement (BinaryTerm l op r) = BinaryTerm (replaced l) op (replaced r)
  where replaced = replaceVar varName replacement

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (IntConstant c)     = IntConstant c
evaluate (Variable v)        = error("Expression is not computable")
evaluate (BinaryTerm l op r) = IntConstant $ apply op (intValue $ evaluate  l) (intValue $ evaluate r)
  where
    apply o = case o of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
