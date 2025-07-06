import Data.List
import Text.Read (readMaybe)

class Semigroup' a where
  (<+>) :: a -> a -> a

class (Semigroup' a) => Monoid' a where
  mempty' :: a

instance Semigroup' Bool where
  (<+>) = (&&)

instance Monoid' Bool where
  mempty' = True

instance Semigroup' Int where
  (<+>) = (*)

instance Monoid' Int where
  mempty' = 0

instance Semigroup' [a] where
  (<+>) = (++)

instance Monoid' [a] where
  mempty' = []

instance Semigroup' (Maybe a) where
  Nothing <+> y = y
  Just x <+> _ = Just x

instance Monoid' (Maybe a) where
  mempty' = Nothing

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <+> (x2, y2) = (x1 <+> x2, y1 <+> y2)

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  mempty' = (mempty', mempty')

instance (Semigroup' b) => Semigroup' (a -> b) where
  f <+> g = \x -> f x <+> g x

mconcat' :: (Monoid' m) => [m] -> m
mconcat' [] = mempty'
mconcat' (x : xs) = x <+> mconcat' xs

mconcat'' :: (Monoid' a) => [a] -> a
mconcat'' = foldr (<+>) mempty'

-- Foldable
class Foldable' f where
  foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' = foldr

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

instance Foldable' Maybe where
  -- foldr' f z = foldr f z . maybeToList'
  foldr' _ z Nothing = z
  foldr' f z (Just x) = f x z

data BTree a = Leaf a | Branch (BTree a) (BTree a)

flatten :: BTree a -> [a]
flatten (Leaf a) = [a]
flatten (Branch l r) = flatten l ++ flatten r

instance Foldable' BTree where
  foldr' f z = foldr f z . flatten

fold' :: (Foldable' f, Monoid' a) => f a -> a
fold' = foldr' (<+>) mempty' -- foldr takes operation then the second which to mutate and third the param jo gayab hai

--- IO exercises

echoLine :: IO ()
echoLine = do
  l <- getLine
  putStrLn l

echoLine' :: IO ()
echoLine' = getLine >>= putStrLn

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
  putStrLn "What's your first name?"
    >> getLine
    >>= \firstName ->
      putStrLn "What's your surname?"
        >> getLine
        >>= \surName ->
          let greeting = "Hello, " ++ firstName ++ " " ++ surName ++ "!"
           in putStrLn greeting

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
  putStrLn "Do you want to go outside?"
    >> getLine
    >>= \l ->
      if l == "yes"
        then putStrLn "It rains!" >> putStrLn "Too, bad!"
        else putStrLn "It still rains! Not so bad!"

choose :: String -> [String] -> IO String
choose question options = do
  putStrLn (question ++ "[" ++ intercalate "," options ++ "]")
  answer <- getLine
  if answer `elem` options
    then return answer
    else do
      putStrLn "Invalid input. Try again!"
      choose question options

choose' :: String -> [String] -> IO String
choose' question options =
  putStrLn (question ++ "[" ++ intercalate "," options ++ "]")
    >> getLine
    >>= \answer ->
      if answer `elem` options
        then
          return answer
        else
          putStrLn "Invalid input. Try again!"
            >> choose question options

options :: [String]
options = ["add", "display", "remove", "quit"]

todo :: IO ()
todo = run []
  where
    run :: [String] -> IO ()
    run items = do
      c <- choose "What do you want to do" options
      option <- getLine
      case option of
        "add" -> do
          putStrLn "Enter the text of todo item:"
          x <- getLine
          run (x : items)
        "display" -> do
          putStrLn $ intercalate "\n" $ map (\(i, s) -> show i ++ ")" ++ s) $ enumerate items
          run items
        "remove" -> do
          putStrLn ""
          putStrLn "Index of item to remove:"
          s <- getLine
          case readMaybe s of
            Nothing -> do
              putStrLn "Invalid index."
              run items
            Just i -> do
              case removeAt i items of
                Nothing -> do
                  putStrLn "Invalid index."
                  run items
                Just (item, items') -> do
                  putStrLn $ "Removed item: " ++ item
                  run items'
        "quit" -> return ()
        _ -> error "Impossible"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt 0 (x : xs) = Just (x, xs)
removeAt i (x : xs) = case removeAt (i - 1) xs of
  Nothing -> Nothing
  Just (y, ys) -> Just (y, x : ys)