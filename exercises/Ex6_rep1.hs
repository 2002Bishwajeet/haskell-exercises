{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Char (isDigit, isLetter)

{-# HLINT ignore "Use tuple-section" #-}

newtype ND a = ND [a]

instance Functor ND where
  fmap f (ND xs) = ND (map f xs)

instance Applicative ND where
  pure :: a -> ND a
  pure a = ND [a]
  (<*>) :: ND (a -> b) -> ND a -> ND b
  (ND fs) <*> (ND xs) = ND [f x | f <- fs, x <- xs] -- both ND are list so iterating each list and adding them

instance Monad ND where
  return = pure
  (>>=) :: ND a -> (a -> ND b) -> ND b
  (ND a) >>= f = ND [y | x <- a, let ND ys = f x, y <- ys]

runND :: ND a -> [a]
runND (ND a) = a

choose :: [a] -> ND a
choose = ND

abort :: ND a
abort = ND []

ex1 :: [Int]
ex1 = runND $ do
  x <- choose [1, 2]
  y <- choose [10, 20]
  return $ x + y

flipCoin :: ND Bool
flipCoin = choose [True, False]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = do
  coin1 <- flipCoin
  coin2 <- flipCoin
  return (coin1, coin2)

type Graph n = [(n, [n])]

type Coloring n c = [(n, c)]

solve :: (Eq n, Eq c) => Graph n -> [c] -> ND (Coloring n c)
solve g colors = f g []
  where
    f [] colorings = return colorings
    f ((n, ns) : g') colorings = do
      c <- choose colors
      if any (\x -> lookup x colorings == Just c) ns
        then abort
        else
          f g' ((n, c) : colorings)

exGraph :: Graph Int
exGraph =
  -- 1
  [ (0, [1, 2]), -- / \
    (1, [3, 0]), -- 0 3
    (2, [3, 0]), -- \ /
    (3, [1, 2]) -- 2
  ]

exColorings :: [Coloring Int String]
exColorings = runND $ solve exGraph ["red", "blue"]

newtype Partial a = Partial (Maybe a)

instance Functor Partial where
  fmap f (Partial mx) = Partial (mapMaybe f mx)

instance Applicative Partial where
  pure a = Partial (Just a)
  (<*>) :: Partial (a -> b) -> Partial a -> Partial b
  (Partial mf) <*> (Partial mx) = Partial (mf >>= \f -> mx >>= \x -> Just (f x))

instance Monad Partial where
  return = pure
  (>>=) :: Partial a -> (a -> Partial b) -> Partial b
  (Partial mx) >>= f = Partial (mx >>= \x -> runPartial (f x))

runPartial :: Partial a -> Maybe a
runPartial (Partial x) = x

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

failure :: Partial a
failure = Partial Nothing

(!?) :: [a] -> Int -> Partial a
[] !? _ = failure
(x : _) !? 0 = return x
(x : xs) !? i = xs !? (i - 1)

getCell :: [[a]] -> Int -> Int -> Partial a
getCell table x y = do
  row <- table !? y
  col <- row !? x
  return col

newtype Exception e a = Exception (Either e a)

runException :: Exception e a -> Either e a
runException (Exception x) = x

instance Functor (Exception e) where
  fmap :: (a -> b) -> Exception e a -> Exception e b
  fmap f (Exception x) = Exception (fmap f x)

instance Applicative (Exception e) where
  pure x = Exception (Right x)
  (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (Exception mf) <*> (Exception mx) = Exception (mf >>= \f -> mx >>= \x -> Right (f x))

instance Monad (Exception e) where
  return = pure
  (>>=) :: Exception e a -> (a -> Exception e b) -> Exception e b
  (Exception mx) >>= f = Exception (mx >>= \x -> runException (f x))

raise :: e -> Exception e a
raise e = Exception (Left e)

withException' :: Maybe a -> e -> Either e a
withException' Nothing e = Left e
withException' (Just x) _ = Right x

withException :: Partial a -> e -> Exception e a
withException (Partial mx) e = Exception (withException' mx e)

validatePassword :: String -> Exception String ()
validatePassword s = do
  if length s < 8
    then raise "Password length less than 8 characters"
    else
      if not (any isDigit s && any isLetter s)
        then raise "Atleast one digit or character"
        else
          return ()

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State x) = x

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (const ((), s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (x, s') = g s in (f x, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\s -> (x, s))