
module Ex1 where

import Data.List (sort)
import Test.QuickCheck
import Data.ByteString (length)
import Distribution.Compat.Prelude (Integer)
import Data.Maybe (Maybe(Nothing))

-- Exercise 1 (Warm up)
maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y

mini :: Integer -> Integer -> Integer
mini x y = if x > y then y else x

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x, y, z) = maxi x (maxi y z)

med :: Integer -> Integer -> Integer -> Integer
med x y z = max3 (mini x y) (mini y z) (mini x z)

-- Testing QuickCheck

-- QuickCheck Tests

prop_mini :: Integer -> Integer -> Bool
prop_mini x y = mini x y == head (sort [x, y])

prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = maxi x y == last (sort [x, y])

prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max3 x y z == last (sort [x, y, z])

prop_max3Tupled :: Integer -> Integer -> Integer -> Bool
prop_max3Tupled x y z = max3Tupled (x, y, z) == last (sort [x, y, z])

prop_med :: Integer -> Integer -> Integer -> Bool
prop_med x y z = med x y z == sort [x, y, z] !! 1

-- Exercise 2 (List Functions)

null' :: [a] -> Bool
null' list  = True
null' _  = False

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x: xs) = x + sum' xs 

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

elem' :: Integer -> [Integer] -> Bool 
elem' _ [] = False
elem' x (y : ys) | x == y    = True
                 | otherwise = elem' x ys

take' :: Integer -> [a] -> [a]
take' n (x: xs) | n > 0 = x : take' (n -1 )xs
take' _ _ = []

drop' :: Integer -> [a] -> [a]
drop' n (x:xs) | n > 0 = drop' (n -1 )xs
drop' _ ns = ns


last' :: [a] -> a
last' [] = error "last' is undefined on the empty list"
last' (x : []) = x
last' (_ : xs) = last' xs

lastSafe' :: [a] -> a
lastSafe' [] = Nothing
lastSafe' (x : []) = Just x
lastSafe' (_ : xs) = lastSafe' xs


init' :: [a] -> [a]
init' [] = error "init' is undefined on the empty list"
init' (_ : []) = []
init' (x : xs) = x : init' xs


initSafe' :: [a] -> [a]
initSafe' [] = Nothing
initSafe' (_ : []) = []
initSafe' (x: xs) = x : initSafe'xs

-- Uses O(n^2) time complexity
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]
-- QuickCheck Tests

