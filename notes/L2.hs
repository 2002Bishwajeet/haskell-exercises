module L2 where

testList :: [Integer]
testList = [1, 2, 3, 4, 5, 6]

-- power :: Integer -> Integer -> Integer
-- power _ 0 = 1
-- power x n = x * power x (n - 1)

-- (x:xs) - here x is head and xs is the rest of the list so x first element and xs last element
-- You can use this to traverse the list and do recursion stuffs

-- Operations in List
-- ++ to add two list , two strings,
-- e.g [1,2,3,4] ++ [4,5,6,7,8]
--  This is fine for small lists but grew expensive for large operations
-- Since it needs to iterate to the end of a list to add stuffs
-- : operator can be used to add items to the front of the list
-- e.g 5: [0,1,2,3,4]

-- [x*2 | x <- [1..10]]
-- This is called Set comprehensions. so before | is a pipe function and the other part on the right is the predicate
-- which specifies the conditions. you can add multiple conditions proceeded by comma
