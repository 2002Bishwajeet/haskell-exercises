{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

import Control.Monad.RWS (modify)
import Control.Monad.Trans.Accum (runAccum)
import Data.Char

-- Define the newtype
newtype ND a = ND [a]

instance Functor ND where
  fmap :: (a -> b) -> ND a -> ND b
  fmap f (ND xs) = ND (map f xs)

instance Applicative ND where
  pure :: a -> ND a
  pure x = ND [x]
  (<*>) :: ND (a -> b) -> ND a -> ND b
  (ND fs) <*> (ND xs) = ND [f x | f <- fs, x <- xs]

instance Monad ND where
  return :: a -> ND a
  return = pure
  (>>=) :: ND a -> (a -> ND b) -> ND b
  (ND xs) >>= f = ND [y | x <- xs, let (ND ys) = f x, y <- ys]

runND :: ND a -> [a]
runND (ND xs) = xs

choose :: [a] -> ND a
choose = ND

abort :: ND a
abort = ND []

ex1 :: [Int]
ex1 = runND $ do
  x <- choose [1, 2]
  y <- choose [10, 20]
  return $ x + y

ex2 :: [Int]
ex2 = runND $ do
  x <- choose [1 .. 10]
  if even x
    then
      return x
    else
      abort

flipCoin :: ND Bool
flipCoin = choose [True, False]

flipTwoCoins :: ND (Bool, Bool)
flipTwoCoins = (,) <$> flipCoin <*> flipCoin -- this is advanced, do the one below, that's easy

-- OR
flipTwoCoins' :: ND (Bool, Bool)
flipTwoCoins' = do
  coin1 <- flipCoin
  coin2 <- flipCoin
  return (coin1, coin2)

type Graph n = [(n, [n])]

type Coloring n c = [(n, c)]

solve :: (Eq n, Eq c) => Graph n -> [c] -> ND (Coloring n c)
solve g colors = f g []
  where
    f [] coloring =
      return coloring
    f ((n, ns) : g') coloring = do
      c <- choose colors
      if any (\n' -> lookup n' coloring == Just c) ns
        then
          abort
        else
          f g' ((n, c) : coloring)

exGraph :: Graph Int
exGraph =
  --                 1
  [ (0, [1, 2]), -- / \
    (1, [3, 0]), -- 0 3
    (2, [3, 0]), -- \ /
    (3, [1, 2]) --   2
  ]

exColorings :: [Coloring Int String]
exColorings = runND $ solve exGraph ["red", "blue"]

-- exColorings is
-- [ [(3,"red"), (2,"blue"), (1,"blue"), (0,"red")]
-- , [(3,"blue"), (2,"red"), (1,"red"), (0,"blue")]

---MAYBE ----

newtype Partial a = Partial (Maybe a)

instance Functor Partial where
  fmap :: (a -> b) -> Partial a -> Partial b
  fmap f (Partial mx) = Partial (mapMaybe f mx)

instance Applicative Partial where
  pure :: a -> Partial a
  pure x = Partial (Just x)
  (<*>) :: Partial (a -> b) -> Partial a -> Partial b
  (Partial mf) <*> (Partial mx) = Partial (mf >>= \f -> mx >>= \x -> Just (f x))

instance Monad Partial where
  return :: a -> Partial a
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
-- xs !? n
--   | n < 0 = failure
--   | n >= length xs = failure
--   | otherwise = return (xs !! n)
-- or
[] !? _ = failure
(x : _) !? 0 = return x
(_ : xs) !? i = xs !? (i - 1)

getCell :: [[a]] -> Int -> Int -> Maybe a
-- getCell [] _ _ = failure
-- getCell (x : xs) 0 j = x !? j
-- getCell xs i 0 = getCell xs (i - 1) 0
-- getCell xs i j = getCell xs (i - 1) (j - 1)

-- or using do notation
getCell table x y = runPartial $ do
  row <- table !? y
  col <- row !? x
  return col

getCell' :: [[a]] -> Int -> Int -> Maybe a
getCell' table x y = runPartial $ do
  row <- table !? y
  row !? x

-- Exception -------------------------------------------------------------------

newtype Exception e a = Exception (Either e a)

runException :: Exception e a -> Either e a
runException (Exception x) = x

instance Functor (Exception e) where
  fmap :: (a -> b) -> Exception e a -> Exception e b
  fmap f (Exception x) = Exception (fmap f x)

instance Applicative (Exception e) where
  pure :: a -> Exception e a
  pure x = Exception (Right x)
  (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (Exception mf) <*> (Exception mx) = Exception (mf >>= \f -> mx >>= \x -> Right (f x))

instance Monad (Exception e) where
  return :: a -> Exception e a
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
validatePassword password = do
  if length password < 8
    then raise "Password must be at least 8 characters long"
    else
      if not (any isDigit password && any isLetter password)
        then raise "Password must contain at least one letter and one digit"
        else return ()

validatePassword' :: String -> Exception String ()
validatePassword' password
  | length password < 8 =
      raise "Password must be at least 8 characters long"
  | not (any isDigit password && any isLetter password) = raise "Password must contain at least one letter and one digit"
  | otherwise = return ()

data MatrixError = InvalidRowIndex | InvalidColIndex

getCell'' :: [[a]] -> Int -> Int -> Exception MatrixError a
getCell'' table x y = do
  row <- withException (table !? y) InvalidRowIndex
  col <- withException (row !? x) InvalidColIndex
  return col

--- State

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State g) = g

mapFirst :: (a1 -> a2) -> ((a1, b) -> (a2, b))
mapFirst f (x, y) = (f x, y)

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  -- fmap f (State g) = State (\s -> let (x, s') = g s in (f x, s'))
  -- fmap f (State g) = State $ \s -> let (x, s') = g s in (f x, s')
  fmap f (State g) = State $ mapFirst f . g

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (\s -> (x, s))
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State g) <*> (State h) = State $ \s ->
    let (f, s') = g s
        (x, s'') = h s'
     in (f x, s'')

instance Monad (State s) where
  return :: a -> State s a
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State g) >>= f = State $ \s ->
    let (x, s') = g s
        (State h) = f x
     in h s'

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (const ((), s))

-- put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

f :: Int -> State String Bool
f x = do
  s <- get
  put (s ++ show x)
  return (x > 0)