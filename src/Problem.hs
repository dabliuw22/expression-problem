module Problem where

data Expr
  = Val Int
  | Add Expr Expr

eval :: Expr -> Int
eval (Val i) = i
eval (Add x y) = eval x + eval y

myExpr :: Expr
myExpr = Add (Val 1) (Add (Val 10) (Val 2))

-- Problem: If we add new types of Expr, we must update all our functions.
