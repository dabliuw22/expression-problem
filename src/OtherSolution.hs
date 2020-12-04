{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module OtherSolution where

class ExprSym repr where
  val :: Int -> repr
  neg :: repr -> repr
  (⊕) :: repr -> repr -> repr

instance ExprSym Int where
  val i = i
  neg n = - n
  x ⊕ y = x + y

instance ExprSym String where
  val i = show i
  neg n = "(-" <> n <> ")"
  x ⊕ y = "(" <> x <> "+" <> y <> ")"

eval :: Int -> Int
eval = id

view :: String -> String
view = id

type Repr a = ExprSym a => a

addExample :: Repr a
addExample = val 4 ⊕ neg (val 2)

-- Extension
class ExprMul repr where
  (⊗) :: repr -> repr -> repr

instance ExprMul Int where
  x ⊗ y = x * y

instance ExprMul String where
  x ⊗ y = "(" <> x <> "*" <> y <> ")"

type ExtRepr a = ExprMul a => Repr a

newMulExample :: ExtRepr a
newMulExample = val 4 ⊕ (neg (val 2) ⊗ val 2)
