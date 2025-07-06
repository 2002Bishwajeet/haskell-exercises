{-# LANGUAGE LambdaCase #-}

import Data.List
import Text.Read (readMaybe)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) =
  if f x
    then x : filter' f xs
    else filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ y [] = y
foldr' f y (x : xs) = f x (foldr' f y xs)

inc :: [Int] -> [Int]
inc = map' (+ 1)

inc' :: [Int] -> [Int]
inc' [] = p
inc' (x : xs) = (x + 1) : inc' xs

evenList :: [Int] -> [Int]
evenList = filter' even

evenList' :: [Int] -> [Int]
evenList' [] = []
evenList' (x : xs)
  | even x = x : evenList' xs
  | otherwise = evenList' xs

shortStrs :: [Int] -> [String]
shortStrs = filter' (\x -> length x <= 2) . map' show

shortStrs' :: [Int] -> [String]
shortStrs' [] = []
shortStrs' (x : xs)
  | length (show x) <= 2 = show x : shortStrs' xs
  | otherwise = shortStrs' xs

and' :: [Bool] -> Bool
and' = foldr' (&&) True

and'' :: [Bool] -> Bool
and'' [] = True
and'' (x : xs) = x && and'' xs

or' :: [Bool] -> Bool
or' = foldr' (||) False

or'' :: [Bool] -> Bool
or'' [] = False
or'' (x : xs) = x || or'' xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x : xs) = f x && all' f xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x : xs) = f x || any' f xs

length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

length'' :: [a] -> Integer
length'' = foldr' (\_ n -> n + 1) 0

idMap :: [a] -> [a]
idMap = map' id

idFilter :: [a] -> [a]
-- idFilter = filter' (\_ -> True)
idFilter = filter' (const True) -- or this

idFold :: [a] -> [a]
idFold = foldr' (:) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' (\x ys -> f x : ys) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr' (\x ys -> if f x then x : ys else ys) []

--- Ex 2

data V3 a = V3 a a a deriving (Show, Eq)

exampleV3 :: V3 Int
exampleV3 = V3 5 3 8

negateV3 :: V3 Int -> V3 Int
negateV3 (V3 x y z) = V3 (negate x) (negate y) (negate z)

addV3 :: V3 Int -> V3 Int -> V3 Int
addV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

mulV3 :: V3 Int -> V3 Int -> V3 Int
mulV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 * x2) (y1 * y2) (z1 * z2)

mapV3 :: (a -> b) -> V3 a -> V3 b
mapV3 f (V3 a b c) = V3 (f a) (f b) (f c)

liftV3 :: (a -> b -> c) -> V3 a -> V3 b -> V3 c
liftV3 f (V3 a b c) (V3 x y z) = V3 (f a x) (f b y) (f c z)

negateV3' :: V3 Int -> V3 Int
negateV3' = mapV3 negate

addV3' :: V3 Int -> V3 Int -> V3 Int
addV3' = liftV3 (+)

mulV3' :: V3 Int -> V3 Int -> V3 Int
mulV3' = liftV3 (*)

subV3 :: V3 Int -> V3 Int -> V3 Int
subV3 = liftV3 (-)

divV3 :: V3 Int -> V3 Int -> V3 Int
divV3 = liftV3 div

-- Tic Tac toe

data Token = X | O | E deriving (Show, Eq)

type Board = [[Token]]

emptyBoard :: Int -> Board
emptyBoard n = replicate n (replicate n E)

isFull :: Board -> Bool
isFull = all (notElem E)

showToken :: Token -> String
showToken X = "X"
showToken O = "O"
showToken E = "_"

showRow :: [Token] -> String
showRow = unwords . map showToken

-- showRow = intercalate " " . map showToken

showBoard :: Board -> String
showBoard b = intercalate "\n" $ map showRow b

updateListM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
updateListM = undefined

mapAtM :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
mapAtM _ _ [] = Nothing
mapAtM 0 f (x : xs) = fmap (: xs) (f x)
mapAtM i f (x : xs) = fmap (x :) (mapAtM (i - 1) f xs)

setToken :: (Int, Int) -> Token -> Board -> Maybe Board
setToken (x, y) t = mapAtM y $ mapAtM x $ \case
  E -> Just t
  _ -> Nothing

winner :: Board -> Token
winner = undefined

main :: IO ()
main = run (emptyBoard 3) X
  where
    run :: Board -> Token -> IO ()
    run b t = do
      putStrLn $ showBoard b ++ "\n"
      putStr $ "Where to place " ++ show t ++ "? "
      line <- getLine
      case readMaybe line of
        Nothing -> do
          putStrLn "Invalid input... Try again!"
          run b t
        Just pos -> do
          putStrLn ""
          case setToken pos t b of
            Nothing -> do
              putStrLn "Invalid position... Try again!"
              run b t
            Just b' -> do
              case winner b' of
                E | isFull b' -> do
                  putStrLn $ showBoard b' ++ "\n"
                  putStrLn "Game over. Everyone looses."
                E -> do
                  run b' (flipTok t)
                t' -> do
                  putStrLn $ showBoard b' ++ "\n"
                  putStrLn $ show t' ++ " is the winner!"

    flipTok :: Token -> Token
    flipTok X = O
    flipTok O = X
    flipTok E = E
