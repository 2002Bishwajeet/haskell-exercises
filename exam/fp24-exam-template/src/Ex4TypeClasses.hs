module Ex4TypeClasses where

import Test.QuickCheck

-- Exercise 4 [10 Points] ------------------------------------------------------

-- The following class defines a `deepSum` operation, which should sum up
-- all the integers occuring directly or indirectly in a value.

class DeepSum a where
  deepSum :: a -> Int

-- With "integers" we mean here the `Int` type and *not* the `Integer` type.
--
-- Implement instances for this class to allow for summing up arbitarily nested
-- lists of integers, e.g.
--
--   deepSum 5 == 5
--   deepSum [1, 2] == 3
--   deepSum [[1, 2], [3]] == 6
--   ...

-- SUBMISSION BEGIN 4

-- Your code goes here

-- SUBMISSION END

