import Data.List

maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y
maxi x y
  | x > y = x
  | otherwise = y

mini :: Integer -> Integer -> Integer
mini x y = if x < y then x else y

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi (maxi x y) z

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x, y, z) = max3 x y z

med :: Integer -> Integer -> Integer -> Integer
med x y z = max3 (mini x y) (mini y z) (mini x z)

-- QuickCheck
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

null' :: [a] -> Bool
null' [] = True
null' _ = False

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- Or
-- sum' xs = foldr (+) 0 xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

-- ALternative with foldr
-- concat' xs = foldr (++) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : xs)
  | x == y = True
  | otherwise = elem' x xs

take' :: Integer -> [Integer] -> [Integer]
take' n (x : xs) | n > 0 = x : take' (n - 1) xs
take' _ _ = []

drop' :: Integer -> [Integer] -> [Integer]

drop n (_ : xs) | n > 0 = drop' (n - 1) xs

drop' _ xs = xs

last' :: [a] -> a
last' [] = error "List is empty MF"
last' [x] = x
last' (_ : xs) = last' xs

lastSafe :: [a] -> Maybe a
lastSafe [] = Nothing
lastSafe [x] = Just x
lastSafe (_ : xs) = lastSafe xs

init' :: [a] -> [a]
init' [] = error "List empty MF"
init' [_] = []
init' (x : xs) = x : init' xs

initSafe :: [a] -> Maybe [a]
initSafe [] = Nothing
initSafe [_] = Just []
initSafe (x : xs) = case initSafe xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

-- This runs in O(n^2)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = f xs []
  where
    f [] ys = ys
    f (x : xs) ys = f xs (x : ys)

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' a (x : xs) = x : a : intersperse' a xs
