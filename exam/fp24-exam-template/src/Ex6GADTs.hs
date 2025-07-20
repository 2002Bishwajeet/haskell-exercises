{-# LANGUAGE GADTs #-}

module Ex6GADTs where

import Test.QuickCheck

-- Exercise 6 [5 Points] -------------------------------------------------------

-- The following GADT describes syntax trees for a tiny language with integers,
-- booleans, a <-operator, and a &&-operator:

data Expr a where
  EInt  :: Int -> Expr Int
  EBool :: Bool -> Expr Bool
  ELess :: Expr Int -> Expr Int -> Expr Bool
  EAnd  :: Expr Bool -> Expr Bool -> Expr Bool

-- Write a function `eval`, which evaluates an `Expr` to its corresponding
-- Haskell value, e.g.
--
--   >>> eval (EInt 5) 
--   5
--
--   >>> eval (EAnd (ELess (EInt 5) (EInt 3))
--                  (EBool True)) 
--   False
--
-- Recall that type inference does not work for GADTs, so you need to also
-- write a type signature for `eval`.

-- SUBMISSION BEGIN 6
eval = undefined
-- SUBMISSION END
