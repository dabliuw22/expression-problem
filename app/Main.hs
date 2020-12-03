{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Solution

main :: IO ()
main = do
  let x :: Expr (Add :+: Val) = val 33 ⊕ val 1
      y :: Expr (Val :+: (Add :+: Mul)) = val 80 ⊗ val 5 ⊕ val 4
  print $ eval x
  print $ eval y
