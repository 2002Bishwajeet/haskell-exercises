module Ex7LambdaCalculus where

import Test.QuickCheck

-- Exercise 7 [20 Points] ------------------------------------------------------

-- Exercise 7.1 [5 Points] -----------------------------------------------------

-- The Church encoding for the natural number 3 is `λf. λx. f (f (f x))`.
--
-- Which of the following definitions corresponds to an addition
-- operator on church encoded natural numbers?
--
-- (A1) λm. λn. λf. λx. m (n f) x
-- (A2) λm. λn. λf. λx. m (n f x) f
-- (A3) λm. λn. λf. λx. m f (n f x)
-- (A4) λm. λn. λf. λx. m x (n f)

data Answer = IDontKnow | A1 | A2 | A3 | A4 deriving (Eq, Show)

-- SUBMISSION BEGIN 7.1
answer :: Answer
answer = IDontKnow
-- SUBMISSION END

-- Exercise 7.2 [15 Points] ----------------------------------------------------

-- The following code describes syntax trees of the lambda calculus.

type Var = String

data Exp = EVar Var
         | ELam Var Exp
         | EApp Exp Exp
         deriving (Show, Eq)

-- Exercise 7.2.1 [5 Points] ---------------------------------------------------

-- Write a function `free`, which returns the free variables of a term.
--
-- It is okay if the same variable occurs multiple times in the output list.

-- SUBMISSION BEGIN 7.2.1
free :: Exp -> [Var]
free = undefined
-- SUBMISSION END

-- Exercise 7.2.2 [5 Points] ---------------------------------------------------

-- Write a function `bound`, which returns the bound variables of a term
--
-- It is okay if the same variable occurs multiple times in the output list.
--
-- You can assume that the variable of each lambda actually occurs in its body.

-- SUBMISSION BEGIN 7.2.2
bound :: Exp -> [Var]
bound = undefined
-- SUBMISSION END

-- Exercise 7.2.3 [5 Points] ---------------------------------------------------

-- Write a function `sub`, which performs capture avoiding substitution on
-- a term, i.e. `sub x e' e` should replace all occurences of `x` in `e` with `e'`.

-- You can make use of the following function to generate fresh variables:

fresh :: [Var] -> Var
fresh xs = head $ filter (`notElem` xs) $ map (\n -> "x" ++ show n) [1::Int ..] 

-- You may want to use the `notElem` function, which is imported by default
-- and checks whether an element is not contained in a list.

-- You get partial points if the function performs substitution, but is not
-- capture-avoiding.

-- SUBMISSION BEGIN 7.2.3
sub :: Var -> Exp -> Exp -> Exp
sub = undefined
-- SUBMISSION END

