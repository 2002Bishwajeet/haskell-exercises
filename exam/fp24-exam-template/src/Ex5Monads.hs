{-# LANGUAGE FlexibleContexts #-}

module Ex5Monads where

import Test.QuickCheck

import Control.Applicative
import Control.Monad.Except (MonadError, throwError, runExcept, runExceptT)
import Control.Monad.State.Strict (MonadState, get, runStateT)

import LibParser

-- Exercise 5 [30 Points] ------------------------------------------------------

-- Exercise 5.1 [5 Points] -----------------------------------------------------

-- Write a function `(.#)`, which composes two monadic functions.

-- SUBMISSION BEGIN 5.1
(.#) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(.#) = undefined
-- SUBMISSION END

-- Exercise 5.2 [5 Points] ----------------------------------------------------

-- This file imports the `MonadExcept` and `MonadState` type classes
-- and related functions from the `mtl` library.
--
-- Write a monadic action `checkZero`, which works for all monads
-- that have an `Int` state and that can throw exceptions of the `Err`
-- type (defined below). The `checkZero` action should
-- inspect the state and throw an `ErrNegative` exception, if the state value
-- is less than zero, and a `ErrPositive` exception, if the state value is
-- larger than zero.

data Err = ErrNegative | ErrPositive deriving (Show, Eq)

-- SUBMISSION BEGIN 5.2
checkZero :: (MonadError Err m, MonadState Int m) => m ()
checkZero = undefined
-- SUBMISSION END

-- Exercise 5.3 [5 Points] ----------------------------------------------------

-- Implement the `return` and `>>=` functions for the `State` monad:

newtype State s a = State { runState :: s -> (s, a) }

-- Normally, this would be done by writing an instance of the `Monad`
-- type class, but to avoid writing also `Functor` and `Applicative` instances,
-- you must implement them as separate functions `returnState` and `bindState`.

-- SUBMISSION BEGIN 5.3
returnState :: a -> State s a
returnState = undefined

bindState :: State s a -> (a -> State s b) -> State s b
bindState = undefined
-- SUBMISSION END

-- Exercise 5.4 [15 Points] ----------------------------------------------------

-- Exercise 5.4.1 [7 Points] ---------------------------------------------------

-- This file imports `LibParser.hs`, which contains the parser combinator
-- library, which we developed in the lecture and exercises.
--
-- Write a parser action, which tries to parse an integer from a string,
-- according to the following grammar:
--
--   <nat> ::= [0-9]+
--   <int> ::= '-'? <nat>
--
-- Hint: in `LibParser.hs` you can find a parser for natural numbers:
--
--   pNat :: Parser Char Int

-- SUBMISSION BEGIN 5.4.1
pInt :: Parser Char Int
pInt = undefined
-- SUBMISSION END

-- Exercise 5.4.2 [8 Points] ---------------------------------------------------

-- Write a parser action, which tries to parse an arithmetic expression
-- consisting of natural numbers, addition, and parentheses.
--
-- Your parser needs to follow the following grammar:
--
--   <exp> ::= <nat> | '(' <exp> '+' <exp> ')'

data Exp = ENat Int | EAdd Exp Exp deriving (Show, Eq)

-- SUBMISSION BEGIN 5.4.2
pExp :: Parser Char Exp
pExp = undefined
-- SUBMISSION END

