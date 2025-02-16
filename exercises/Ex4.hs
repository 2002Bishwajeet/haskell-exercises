import Data.List (intercalate)
import Distribution.Compat.Prelude (exitSuccess)
import Text.Read (readMaybe)

{- Exercise 1 (Type Classes) -}

class Semigroup' a where
  (<+>) :: a -> a -> a

class (Semigroup' a) => Monoid' a where
  mempty' :: a

-- Bool Types
instance Semigroup' Bool where
  (<+>) :: Bool -> Bool -> Bool -- Yes we can also define for better clarity but no you can't change it
  (<+>) = (&&)

instance Monoid' Bool where
  mempty' = True

ex1 :: Bool
ex1 = True <+> False <+> False --  Ans False and follows associtavity

-- Int Types
instance Semigroup' Int where
  (<+>) = (+) -- Notice we omit the parameters cause we can write also like this

instance Monoid' Int where
  mempty' = 0

ex2 :: Int
ex2 = 100 <+> 25 <+> 3 -- 128

--   List Types

instance Semigroup' [a] where
  (<+>) = (++)

instance Monoid' [a] where
  mempty' = []

ex3 :: [Int]
ex3 = [3, 4, 5] <+> [6, 7] <+> [9] -- [3,4,5,6,7,9]

instance Semigroup' (Maybe a) where
  Nothing <+> y = y -- Notice we used parameters here cause our operators needs two parameters
  Just x <+> _ = Just x

instance Monoid' (Maybe a) where
  mempty' = Nothing

ex4 :: Maybe Int
ex4 = Just 4 <+> Nothing <+> Just 5

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <+> (x2, y2) = (x1 <+> x2, y1 <+> y2)

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  mempty' = (mempty', mempty')

ex5 :: (Bool, Int)
ex5 = (True, 100) <+> (False, 20) <+> (True, 1) -- => (False, 123)

ex5' :: (Bool, (Int, Int))
ex5' = (True, (100, 1)) <+> (False, (20, 20)) <+> (True, (1, 300)) -- => (False, (123, 321))

instance (Semigroup' b) => Semigroup' (a -> b) where
  (f <+> g) x = f x <+> g x

instance (Monoid' b) => Monoid' (a -> b) where
  mempty' _ = mempty'

ex6 :: Int -> Bool
ex6 = (> 5) <+> (< 10)

ex6' :: Int -> Int
ex6' = (+ 5) <+> (+ 10)

mconcat' :: (Monoid' m) => [m] -> m
mconcat' [] = mempty'
mconcat' (x : xs) = x <+> mconcat' xs

mconcat'' :: (Monoid' a) => [a] -> a
mconcat'' = foldr (<+>) mempty' -- we can also do this as foldr function folds everything from right

-- Foldable

class Foldable' f where
  foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' = foldr

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

instance Foldable' Maybe where
  foldr' f z = foldr' f z . maybeToList

data BTree a = Leaf a | Branch (BTree a) (BTree a)

flatten :: BTree a -> [a]
flatten (Leaf a) = []
flatten (Branch l r) = flatten l ++ flatten r

instance Foldable' BTree where
  foldr' f z = foldr' f z . flatten

fold' :: (Foldable' f, Monoid' a) => f a -> a
fold' = foldr' (<+>) mempty'

example1 :: Int
example1 = fold' [100, 2, 3]

example2 :: (Int, Bool)
example2 = fold' [(1, True), (30, False), (50, False)] -- (153, False)

example3 :: (Bool, Int)
example3 = fold' (Just (True, 5))

example4 :: (Bool, Int)
example4 = fold' Nothing

example5 :: (Bool, Int)
example5 = fold' $ Branch (Leaf (True, 1)) (Leaf (False, 2))

--- IO

echoLine :: IO ()
echoLine = do
  l <- getLine
  putStrLn l

echoLine' :: IO ()
-- echoLine' = getLine >>= \name -> putStrLn name
echoLine' = getLine >>= putStrLn -- since the value of getLine can be directly passed to putStrLn

greet :: IO ()
greet = do
  putStrLn "What's your name"
  l <- getLine
  putStrLn ("Hello, " ++ l ++ "!")

greet' :: IO ()
greet' = putStrLn "What's your name" >> getLine >>= \x -> putStrLn ("Hello, " ++ x ++ "!")

greetFormal :: IO ()
greetFormal = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your surname?"
  surName <- getLine
  let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!"
  putStrLn greeting

greetFormal' :: IO ()
greetFormal' =
  putStrLn "What's your firstName"
    >> getLine
    >>= ( \firstName ->
            putStrLn "What's your surname?"
              >> getLine
              >>= \surName ->
                let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!"
                 in putStrLn greeting
        )

choices :: IO ()
choices = do
  putStrLn "Do you want to go outside?"
  l <- getLine
  if l == "yes"
    then do
      putStrLn "It rains!"
      putStrLn "Too, bad!"
    else
      putStrLn "It still rains! Not so bad!"

choices' :: IO ()
choices' =
  putStrLn "Do you want to go outside"
    >> getLine
    >>= \l -> if l == "yes" then putStrLn "It rains!" >> putStrLn "Too, bad" else putStrLn "It still rains! Not so bad!"

choose :: String -> [String] -> IO String
choose question choices = do
  putStrLn $ question ++ "[" ++ intercalate "|" choices ++ "]"
  l <- getLine
  if l `elem` choices
    then
      return l
    else do
      putStrLn "Invalid Input, Try again!"
      choose question choices

todoOptions :: [String]
todoOptions = ["add", "display", "remove", "quit"]

todo :: IO ()
todo = run []
  where
    run :: [String] -> IO ()
    run items = do
      putStrLn ""
      c <- choose "What do you want to do?" todoOptions
      case c of
        "add" -> do
          putStrLn "Enter text of todo item:"
          item <- getLine
          run (item : items)
        "display" -> do
          putStrLn ""
          putStr $ intercalate "\n" $ map (\(i, s) -> show i ++ ")" ++ s) $ enumerate items
        "remove" -> do
          putStrLn "Put index to remove:"
          index <- getLine
          case readMaybe index of
            Nothing -> do
              putStrLn "Invalid index"
              run items
            Just i -> do
              case removeAt i items of
                Nothing -> do
                  putStrLn "Invalid index."
                  run items
                Just (item, items') -> do
                  putStrLn $ "Removed item:" ++ item
                  run items'
        "quit" -> do
          return ()
        _ -> error "Something is wrong"

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt 0 (x : xs) = Just (x, xs)
removeAt i (x : xs) = case removeAt (i - 1) xs of
  Nothing -> Nothing
  Just (y, ys) -> Just (y, x : ys)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]
