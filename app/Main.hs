{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Solution

main :: IO ()
main = do
  let x :: Expr (Add :+: Val) = val 33 ⊕ val 1
  print $ eval x
