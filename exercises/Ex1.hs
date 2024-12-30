module Ex1 where

import Data.List (sort)

-- Exercise 1

maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y

mini :: Integer -> Integer -> Integer
mini x y = if x < y then x else y

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x, y, z) = max3 x y z

med :: Integer -> Integer -> Integer -> Integer
med x y z = max3 (mini x y) (mini y z) (mini z x)

-- Testing QuickCheck

-- QuickCheck Tests

prop_maxi :: Integer -> Integer -> Bool
prop_maxi x y = maxi x y == last (sort [x, y])

prop_mini :: Integer -> Integer -> Bool
prop_mini x y = mini x y == head (sort [x, y])

prop_max3 :: Integer -> Integer -> Integer -> Bool
prop_max3 x y z = max3 x y z == last (sort [x, y, z])

prop_max3Tupled :: (Integer, Integer, Integer) -> Bool
prop_max3Tupled (x, y, z) = max3Tupled (x, y, z) == last (sort [x, y, z])

prop_med :: Integer -> Integer -> Integer -> Bool
prop_med x y z = med x y z == sort [x, y, z] !! 1

-- Exercise 2

null' :: [a] -> Bool
null' [] = True
null' _ = False

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x : xs) = x + sum' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

elem' :: Integer -> [Integer] -> Bool
elem' _ [] = False
-- elem' x (y: xs) = if x == y then True else elem' x xs 
elem' x (y: xs) = (x == y) || elem' x xs
-- elem' x (y:xs) | x == y = True
--                | otherwise = elem' x xs

take' :: Integer -> [a] -> [a]
take' x (y: xs) | x > 0 = y : take' (x-1) xs
take' _ _ = []

drop' :: Integer -> [a] -> [a]
drop' n (x: xs) | n > 0 = drop' (n-1) xs
drop' _ xs = xs

last' :: [a] -> a
last' [] =  error "last' is undefined on the empty list"
last' [x] = x
last' (_ : xs) = last' xs

lastSafe :: [a] -> Maybe a
lastSafe [] = Nothing
lastSafe [x] = Just x
-- lastSafe (x: []) = Just x  -- you can also do this
lastSafe (_: xs) = lastSafe xs

init' :: [a] -> [a]
init' [] = error "init' is undefined on the empty list"
init' [_] = []
init' (x : xs) = x : init' xs

initSafe :: [a] -> Maybe [a]
initSafe [] = Nothing
initSafe [_] = Just []
initSafe (x : xs) = case initSafe xs of
         Nothing -> Nothing
         Just ys -> Just (x : ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x: xs) (y: xz) = (x,y) : zip' xs xz

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x: xs) = reverse' xs ++ [x]


intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [y] = [y]
intersperse' n (x: xs) = x: n : intersperse' n xs



