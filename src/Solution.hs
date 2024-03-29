{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Solution where

data Expr f = In (f (Expr f))

newtype Val x = Val Int

type IntExpr = Expr Val

data Add v = Add v v

type AddExpr = Expr Add

-- Coproduct Solution
data (f :+: g) e = Inl (f e) | Inr (g e) -- data Coproduct f g e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add)
addExample =
  In
    ( Inr
        ( Add
            (In (Inl (Val 100)))
            (In (Inl (Val 2)))
        )
    ) -- Add (Val 100) (Val 2)

instance Functor Val where
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- Smart constructors
class (Functor fa, Functor fb) => fa :≺: fb where
  inj :: fa a -> fb a

instance Functor f => f :≺: f where
  inj = id

instance (Functor f, Functor g) => f :≺: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :≺: g) => f :≺: (h :+: g) where
  inj = Inr . inj

inject :: (g :≺: f) => g (Expr f) -> Expr f
inject = In . inj

(⊕) :: (Add :≺: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

infixl 4 ⊕

val :: (Val :≺: f) => Int -> Expr f
val x = inject (Val x)

newAddExample :: Expr (Val :+: Add)
newAddExample = val 33 ⊕ val 1

-- Extension
data Mul v = Mul v v

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

(⊗) :: (Mul :≺: f) => Expr f -> Expr f -> Expr f
x ⊗ y = inject (Mul x y)

infixl 5 ⊗

mulExample :: Expr (Val :+: (Add :+: Mul))
mulExample = val 80 ⊗ val 5 ⊕ val 4
