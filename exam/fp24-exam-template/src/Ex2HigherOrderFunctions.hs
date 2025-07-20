module Ex2HigherOrderFunctions where

import Test.QuickCheck

-- Exercise 2 [15 Points] ------------------------------------------------------

-- Implement `map`, `filter` and `fold` functions for binary trees.
--
-- The `filter` function should replace an entire `Branch l x r`
-- with a `Leaf`, if the predicate returns `False` for `x`.

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Show, Eq)

-- Exercise 2.1 [5 Points] -----------------------------------------------------

-- SUBMISSION BEGIN 2.1
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined
-- SUBMISSION END

-- Exercise 2.2 [5 Points] -----------------------------------------------------

-- SUBMISSION BEGIN 2.2
filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree = undefined
-- SUBMISSION END

-- Exercise 2.3 [5 Points] -----------------------------------------------------

-- SUBMISSION BEGIN 2.3
foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree = undefined
-- SUBMISSION END

