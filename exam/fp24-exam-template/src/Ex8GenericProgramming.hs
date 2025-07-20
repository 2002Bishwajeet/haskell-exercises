{-# LANGUAGE TypeOperators #-}

module Ex8GenericProgramming where

import Test.QuickCheck

-- Exercise 8 [10 Points] ------------------------------------------------------

-- The file `LibGenerics.hs` provides an implementation of the generic
-- programming library, which we used in the last lectures and exercise.

import LibGenerics

-- The following datatype describes a tree with integer leafs:

data IntTree = Leaf Int | Branch IntTree IntTree deriving (Show, Eq)

-- Define a generic encoding of the `IntTree` datatype as a functor `IntTreeF`
-- by using the functor combinators from `LibGenerics.hs` and write conversion
-- functions `toGen` and `fromGen` which convert between `IntTree` and
-- `Fix IntTreeF` values.

-- SUBMISSION BEGIN 8
type IntTreeF = Id -- replace `Id` with your solution

toGen :: IntTree -> Fix IntTreeF
toGen = undefined

fromGen :: Fix IntTreeF -> IntTree
fromGen = undefined
-- SUBMISSION END

