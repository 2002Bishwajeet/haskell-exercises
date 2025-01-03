module Ex2 where

-- Exercise 1

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | not (f x) = []

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f x [] = x
foldr' f x [ys] = f ys x
foldr' f x (y : ys) = f y (foldr' f x ys)

inc :: [Integer] -> [Integer]
inc [] = []
inc (x : xs) = (x + 1) : inc xs

inc' :: [Integer] -> [Integer]
inc' x = map' (+ 1) x

-- inc' = map' (+1)

evenList :: [Integer] -> [Integer]
evenList (x : xs)
  | even x = x : evenList xs
  | otherwise = evenList xs

evenList' :: [Integer] -> [Integer]
evenList' x = filter' even x

-- evenList' = filter' even -- can also be done in this way

shortStrs :: [Integer] -> [String]
shortStrs [] = []
shortStrs (x : xs)
  | length (show x) <= 2 = show x : shortStrs xs
  | otherwise = shortStrs xs

shortStrs' :: [Integer] -> [String]
-- shortStrs' x = map show (filter' (\y -> length (show y) <= 2) x) orrrr
shortStrs' = filter (\s -> length s <= 2) . map show -- same as f. g =  f (g (x)) so here 3 f g h so f(g(h(x)))
--  This is called function composition

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

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr' (\x b -> f x && b) True

all''' :: (a -> Bool) -> [a] -> Bool
all''' f = foldr' (&&) True . map' f

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x : xs) = f x || all' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = foldr' (\x b -> f x || b) False

any''' :: (a -> Bool) -> [a] -> Bool
any''' f = foldr' (||) False . map' f

length' :: [a] -> Integer
length' [] = 0
length' (x : xs) = 1 + length' xs

length'' :: [a] -> Integer
length'' = foldr' (\x -> (+) 1) 0

idmap :: [a] -> [a]
idmap = map' (\x -> x)

idmap' :: [a] -> [a]
idmap' = map' id

{-
In Haskell, id is a standard library function defined in the Prelude module.
It is a simple function that returns its argument unchanged.
The id function is often used in higher-order functions and as a default or placeholder function.
 -}

idFilter :: [a] -> [a]
idFilter = filter' (\_ -> True)

idFold :: [a] -> [a]
idFold = foldr' (:) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' (\x ac -> f x : ac) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr' (\x ac -> if f x then x : ac else ac) []

data V3 a = V3 a a a deriving (Show, Eq)

negateV3 :: V3 Int -> V3 Int
negateV3 (V3 x y z) = V3 (negate x) (negate y) (negate z)

addV3 :: V3 Int -> V3 Int -> V3 Int
addV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

mulV3 :: V3 Int -> V3 Int -> V3 Int
mulV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)

mapV3 :: (a -> b) -> V3 a -> V3 b
mapV3 f (V3 x y z) = V3 (f x) (f y) (f z)

liftV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
liftV3 f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)

-- Exercise 3 (Tic Tac toe) in other code file
