map' :: (a -> b) -> [a] -> [b]
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ y [] = y
foldr' f y (x : xs) = f x (foldr' f y xs)

inc' :: [Integer] -> [Integer]
inc' [] = []
inc' (x : xs) = (x + 1) : inc' xs

inc'' :: [Integer] -> [Integer]
inc'' = map (+ 1)

evenList :: [Integer] -> [Integer]
evenList [] = []
evenList (x : xs)
  | even x = x : evenList xs
  | otherwise = evenList xs

evenList' :: [Integer] -> [Integer]
evenList' = filter' even

shortStrs :: [Integer] -> [String]
shortStrs (x : xs)
  | length (show x) > 2 = show x : shortStrs xs
  | otherwise = shortStrs xs

shortStrs' :: [Integer] -> [String]
shortStrs' = filter' (\x -> length x <= 2) . map show

and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) = x && and' xs

and'' :: [Bool] -> Bool
and'' = foldr' (&&) True

or' :: [Bool] -> Bool
or' [] = False
or' (x : xs) = x || or' xs

or'' :: [Bool] -> Bool
or'' = foldr' (||) False

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x : xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x : xs) = f x || any' f xs

length' :: [a] -> Integer
length' [] = 0
length' (x : xs) = 1 + length' xs

idMap :: [a] -> [a]
idMap = map' (\x -> x)

idFilter :: [a] -> [a]
-- idFilter = filter' (\_ -> True)
idFilter = filter' (const True)

idFold :: [a] -> [a]
idFold = foldr (:) []

data V3 a = V3 a a a deriving (Show, Eq)

mapV3 :: (a -> b) -> V3 a -> V3 b
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

liftV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
liftV3 f (V3 x y z) (V3 a b c) = V3 (f x a) (f y b) (f z c)