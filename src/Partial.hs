module Partial where

newtype Val = Val Int deriving (Show)

data Add x b = Add x b deriving (Show)

class Expr e

instance Expr Val

instance (Expr x, Expr y) => Expr (Add x y)

class (Expr e) => Eval e where
  eval :: e -> Int

instance Eval Val where
  eval (Val value) = value

instance (Eval ex, Eval ey) => Eval (Add ex ey) where
  eval (Add x y) = eval x + eval y

myExpr :: Add Val (Add Val Val)
myExpr = Add (Val 5) (Add (Val 10) (Val 2))

-- Extension
data Sub x y = Sub x y deriving (Show)

data Mul x y = Mul x y deriving (Show)

instance (Expr x, Expr y) => Expr (Sub x y)

instance (Expr x, Expr y) => Expr (Mul x y)

instance (Eval ex, Eval ey) => Eval (Sub ex ey) where
  eval (Sub x y) = eval x - eval y

instance (Eval ex, Eval ey) => Eval (Mul ex ey) where
  eval (Mul x y) = eval x * eval y

myNewExpr :: Mul (Add Val (Add Val Val)) Val
myNewExpr = Mul myExpr (Val 1)

-- Problem: Type generalization (Val, Add, Sub, Mul, etc), since Expr is a typeclass.
-- myExpr type is (Add Val (Add Val Val)), myNewExpr type is (Mul (Add Val (Add Val Val)) Val)
