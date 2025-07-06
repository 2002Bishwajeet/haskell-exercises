import Data.Char

newtype ND a = ND [a] deriving (Show)

runND :: ND a -> [a]
runND (ND a) = a

choose :: [a] -> ND a
choose = ND

abort :: ND a
abort = choose []

instance Functor ND where
  fmap f (ND xs) = ND (map f xs)

instance Applicative ND where
  pure a = ND [a]
  (ND fs) <*> (ND xs) = ND [f x | f <- fs, x <- xs]

instance Monad ND where
  return = pure
  (>>=) :: ND a -> (a -> ND b) -> ND b
  (ND xs) >>= f = ND [y | x <- xs, let (ND ys) = f x, y <- ys]

ex1 :: [Int]
ex1 = runND $ do
  x <- choose [1, 2]
  y <- choose [10, 20]
  return $ x + y

-- ex1 == [11, 21, 12, 22]
ex2 :: [Int]
ex2 = runND $ do
  x <- choose [1 .. 10]
  if even x
    then
      return x
    else
      abort

-- ex2 == [2, 4, 6, 8, 10]

flipCoin :: ND Bool
flipCoin = choose [True, False]

flipTwoCoin :: ND (Bool, Bool)
flipTwoCoin = do
  flip1Coin <- flipCoin
  flip2Coin <- flipCoin
  return (flip1Coin, flip2Coin)

type Graph n = [(n, [n])]

type Coloring n c = [(n, c)]

solve :: (Eq n, Eq c) => Graph n -> [c] -> ND (Coloring n c)
solve g colors = solve' g [] -- create an arbitrary function
  where
    solve' [] coloring = return coloring
    solve' ((n, ns) : g') coloring = do
      -- deserialse node and connected nodes
      c <- choose colors -- iterate through the color list
      if any (\n' -> lookup n' coloring == Just c) ns -- lookup for the node in ns file and compare with coloring
        then abort
        else
          solve' g' ((n, c) : coloring)

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

mapMaybe :: (a -> b) -> (Maybe a -> Maybe b)
mapMaybe _ Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

instance Functor Partial where
  fmap :: (a -> b) -> Partial a -> Partial b
  fmap f (Partial mx) = Partial (mapMaybe f mx)

instance Applicative Partial where
  pure a = Partial (Just a)
  (<*>) = undefined

instance Monad Partial where
  return = pure
  Partial mx >>= f = Partial (mx >>= \x -> runPartial (f x))

runPartial :: Partial a -> Maybe a
runPartial (Partial x) = x

failure :: Partial a
failure = Partial Nothing

(!?) :: [a] -> Int -> Partial a
[] !? _ = failure
(x : _) !? 0 = return x -- uses from Monad
(x : xs) !? n = xs !? (n - 1)

getCell :: [[a]] -> Int -> Int -> Partial a
getCell table x y = do
  -- y is row x is column
  row <- table !? y
  row !? x

newtype Exception e a = Exception (Either e a)

runException :: Exception e a -> Either e a
runException (Exception x) = x

raise :: e -> Exception e a
raise e = Exception (Left e)

withException' :: Maybe a -> e -> Either e a
withException' Nothing e = Left e
withException' (Just a) _ = Right a

withException :: Partial a -> e -> Exception e a
withException (Partial a) e = Exception (withException' a e)

instance Functor (Exception e) where
  fmap f (Exception ea) = Exception (fmapEither f ea)
    where
      fmapEither g (Left e) = Left e
      fmapEither g (Right a) = Right (g a)

instance Applicative (Exception e) where
  pure a = Exception (Right a)
  (<*>) :: Exception e (a -> b) -> Exception e a -> Exception e b
  (Exception ef) <*> (Exception ea) = Exception $ case ef of
    Left e -> Left e
    Right f -> case ea of
      Left e' -> Left e'
      Right a -> Right (f a)

instance Monad (Exception e) where
  return = pure
  (Exception ea) >>= f = case ea of
    Left e -> Exception (Left e)
    Right a -> f a

validatePassword :: String -> Exception String ()
validatePassword password
  | length password < 8 = raise "Password is less than 8 character"
  | not (any isDigit password && any isAlpha password) = raise "Password must contain atleast one letter and digit"
  | otherwise = return ()

data TableException = InvalidRowIndex | InvalidColIndex
  deriving (Show, Eq)

getCell' :: [[a]] -> Int -> Int -> Exception TableException a
getCell' table x y = do
  row <- table !? y `withException` InvalidRowIndex
  row !? x `withException` InvalidColIndex

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State s) = s

instance Functor (State s) where
  fmap f (State s) = State $ \x ->
    let (x', s') = s x
     in (f x', s')

instance Applicative (State s) where
  --   pure a = State (a,)
  pure a = State $ \s -> (a, s)

  --   <*> :: State s (a -> b) -> State s a -> State s b
  (State sf) <*> (State sa) = State $ \s ->
    let (f, s') = sf s
        (a, s'') = sa s'
     in (f a, s'')

instance Monad (State s) where
  return = pure
  State fs >>= f = State $ \s ->
    let (a, s') = fs s
        State sb = f a
        (b, s'') = sb s'
     in (b, s'')

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  x <- get
  put (f x)

increment :: State Int Int
increment = State $ \s -> (s, s + 1)

--- WhilteInterep code shit

type Var = String

data Val
  = VInt Int
  | VBool Bool
  | VUnit
  | VError String
  deriving (Eq, Show)

data Op = Add | Sub | Less
  deriving (Eq, Show)

data Expr
  = EVar Var
  | EVal Val
  | EOp Expr Op Expr
  | EAssign Var Expr
  | EWhile Expr Expr
  | ESeq Expr Expr
  deriving (Eq, Show)

type Env = [(Var, Val)]

-- Semantics of an operator (how to apply an operator to values)
semOp :: Op -> Val -> Val -> Val
semOp Add (VInt x) (VInt y) = VInt (x + y)
semOp Sub (VInt x) (VInt y) = VInt (x - y)
semOp Less (VInt x) (VInt y) = VBool (x < y)
semOp _ (VError e) _ = VError e
semOp _ _ (VError e) = VError e
semOp _ _ _ = VError "Operator cannot handle those arguments"

insertOrUpdate :: Var -> Val -> Env -> Env
insertOrUpdate x v [] = [(x, v)]
insertOrUpdate x v ((x', v') : env)
  | x == x' = (x, v) : env
  | otherwise = (x', v') : insertOrUpdate x v env

eval :: Expr -> Env -> (Val, Env)
eval (EVar x) env = case lookup x env of
  Just v -> (v, env)
  Nothing -> (VError $ "Variable '" ++ x ++ "' is not defined.", env)
eval (EVal v) env = (v, env)
eval (EOp e1 o e2) env =
  let (v1, env') = eval e1 env
   in let (v2, env'') = eval e2 env'
       in (semOp o v1 v2, env'')
eval (EAssign x e) env =
  let (v, env') = eval e env
   in (VUnit, insertOrUpdate x v env')
eval (EWhile e1 e2) env =
  let (v1, env') = eval e1 env
   in case v1 of
        VBool False -> (VUnit, env')
        VBool True -> eval (ESeq e2 (EWhile e1 e2)) env'
        VError e -> (VError e, env')
        _ -> (VError "While condition was not a bool", env')
eval (ESeq e1 e2) env =
  let (_, env') = eval e1 env
   in eval e2 env'

eval' :: Expr -> State Env Val
eval' (EVar x) = do
  env <- get
  case lookup x env of
    Just v -> return v
    Nothing -> return $ VError $ "Variable '" ++ x ++ "' is not defined."
eval' (EVal v) = return v
eval' (EOp e1 o e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  return (semOp o v1 v2)
eval' (EAssign x e) = do
  v <- eval' e
  modify $ insertOrUpdate x v
  return VUnit
eval' (EWhile e1 e2) = do
  v1 <- eval' e1
  case v1 of
    VBool False -> return VUnit
    VBool True -> eval' $ ESeq e2 (EWhile e1 e2)
    VError e -> return $ VError "While condition was not a bool"
eval' (ESeq e1 e2) = do
  _ <- eval' e1
  eval' e2
