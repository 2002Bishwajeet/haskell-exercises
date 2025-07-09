import Data.ByteString (intersperse)

maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y

mini :: Integer -> Integer -> Integer
mini x y = if x > y then y else x

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = maxi x (maxi y z)

max3Tupled :: (Integer, Integer, Integer) -> Integer
max3Tupled (x, y, z) = max3 x y z

med :: Integer -> Integer -> Integer -> Integer
med x y z = max3 (mini x y) (mini y z) (mini z x)

--- Exercise 2

null' :: [a] -> Bool
null' [] = True
null' _ = False

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x : xs) = x + sum' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x : xs) = x ++ concat' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y : xs) = (x == y) || elem' x xs

take' :: Integer -> [l] -> [l]
take' 0 x = x
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Integer -> [l] -> [l]
drop' 0 x = x
drop' n (x : xs) = drop' (n - 1) xs

last' :: [a] -> a
last' [] = error "List is empty"
last' [x] = x
last' (_ : xs) = last' xs

lastSafe :: [a] -> Maybe a
lastSafe [] = Nothing
lastSafe [x] = Just x
lastSafe (_ : xs) = lastSafe xs

init' :: [a] -> [a]
init' [] = error "List empty Bitch"
init' [_] = []
init' (x : xs) = x : init' xs

initSafe :: [a] -> Maybe [a]
initSafe [] = Nothing
initSafe [_] = Just []
initSafe (x : xs) = case initSafe xs of
  Nothing -> Nothing
  Just ys -> Just (x : ys)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' a (x : xs) = x : a : intersperse' a xs