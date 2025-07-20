module Ex1FunctionsListsRecursion where

import Test.QuickCheck

-- Exercise 1 [20 Points] ------------------------------------------------------

-- Exercise 1.1 [5 Points] -----------------------------------------------------

-- The following datatype extends the boolean datatype with a third constructor
-- for uncertainty: A `Trool` can be either true (`T`), false (`F`), or unknown (`U`).
--
-- Write a function `troolAnd`, which behaves like `(&&)` for `Bool`, but also
-- covers the cases for uncertainty: unknown and false is false, but
-- unknown and true/unknown remains unknown.

data Trool = T | F | U deriving (Show, Eq)

-- SUBMISSION BEGIN 1.1
troolAnd :: Trool -> Trool -> Trool
troolAnd = undefined
-- SUBMISSION END

-- Exercise 1.2 [5 Points] -----------------------------------------------------

-- Write a recursive function `fac`, which computes the factorial of a positive
-- integer, e.g.
--
--   fac 5 == 5 * 4 * 3 * 2 * 1 == 120

-- SUBMISSION BEGIN 1.2
fac :: Int -> Int
fac = undefined
-- SUBMISSION END

-- Exercise 1.3 [5 Points] -----------------------------------------------------

-- The following datatype describes a binary tree.

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving (Show, Eq)

-- Write a function `sumTree`, which sums up all the integers in a binary tree
-- that contains integers.

-- SUBMISSION BEGIN 1.3
sumTree :: Tree Int -> Int
sumTree = undefined
-- SUBMISSION END

-- Exercise 1.4 [10 Points] ----------------------------------------------------

-- Write a function `countInits` which takes a list of strings and counts how
-- many words start with the same letter, e.g.
--
--   >>> countInits ["foo", "bar", "baz", ""]
--   [('f', 1), ('b', 2)]
--
-- Make sure that
--
-- - words, which are the empty string, are ignored; and
--
-- - the resulting list does not contain multiple
--   pairs with the same letter, e.g.
--
--     >>> countInits ["foo", "bar", "baz", ""]
--     [('f', 1), ('b', 2), ('b', 1)]
--   
--   is *not* allowed.
-- 
-- Hint: You might want to use the `lookup` function, but there are multiple
-- ways to solve this exercise.

-- SUBMISSION BEGIN 1.4
countInits :: [String] -> [(Char, Int)]
countInits = undefined
-- SUBMISSION END

