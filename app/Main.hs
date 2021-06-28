{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified OtherSolution as O
import Solution
  ( Add,
    Expr,
    Mul,
    Val,
    eval,
    val,
    (⊕),
    (⊗),
    type (:+:),
  )

main :: IO ()
main = do
  let x :: Expr (Add :+: Val) = val 33 ⊕ val 1
      y :: Expr (Val :+: (Add :+: Mul)) = (val 80 ⊗ val 5 ⊕ val 4 ⊕ val 1) ⊗ val 2
  print $ eval x
  print $ eval y
  print $ O.eval O.newMulExample
  print $ O.view O.newMulExample
