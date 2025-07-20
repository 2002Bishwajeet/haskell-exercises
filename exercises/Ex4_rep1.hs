class Semigroup' a where
  (<+>) :: a -> a -> a

class (Semigroup' a) => Monoid' a where
  mempty' :: a

instance Semigroup' Bool where
  (<+>) = (&&)

instance Monoid' Bool where
  mempty' = True

instance Semigroup' Int where
  (<+>) = (+)

instance Monoid' Int where
  mempty' = 0

instance Semigroup' [a] where
  (<+>) = (++)

instance Monoid' [a] where
  mempty' = []

instance Semigroup' (Maybe a) where
  Nothing <+> y = y
  Just y <+> _ = Just y

instance (Semigroup' a, Semigroup' b) => Semigroup' (a, b) where
  (x1, y1) <+> (x2, y2) = (x1 <+> x2, y1 <+> y2)

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
  mempty' = (mempty', mempty')

instance (Semigroup' b) => Semigroup' (a -> b) where
  (f <+> g) x = f x <+> g x

mconcat' :: (Monoid' m) => [m] -> m
mconcat' [] = mempty'
mconcat' (x : xs) = x <+> mconcat' xs

class Foldable' f where
  foldr' :: (a -> b -> b) -> b -> f a -> b

instance Foldable' [] where
  foldr' = foldr

-- instance Foldable' Maybe where

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
greet' = putStrLn "What's your name" >> getLine >>= (\l -> putStrLn ("Hello, " ++ l ++ "!"))

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
      putStrLn "What's your surname"
        >> getLine
        >>= \lastName ->
          let greeting = "Hello, " ++ firstName ++ " " ++ lastName ++ "!"
           in putStrLn greeting
