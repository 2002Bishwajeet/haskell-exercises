{-# LANGUAGE GADTs #-}

import Control.Monad.Trans

data Term = I Integer | B Bool | Add Term Term | EqL Term Term
  deriving (Eq, Show)

data Value = Int Integer | Bool Bool
  deriving (Show)

eval' :: Term -> Value
eval' (I n) = Int n
eval' (B b) = Bool b
eval' (Add t t') = case (eval' t, eval' t') of
  (Int i, Int i2) -> Int (i + i2)
  _ -> error "Type mismatch"
eval' (EqL t t') = case (eval' t, eval' t') of
  (Int i, Int i2) -> Bool (i == i2)
  (Bool b, Bool b2) -> Bool (b == b2)
  _ -> error "Type mismatch"

data Term' a where
  I' :: Integer -> Term' Integer
  B' :: Bool -> Term' Bool
  Add' :: Term' Integer -> Term' Integer -> Term' Integer
  EqL' :: (Eq x) => Term' x -> Term' x -> Term' Bool

eval :: Term' a -> a
eval (I' i) = i
eval (B' b) = b
eval (Add' t t') = eval t + eval t'
eval (EqL' t t') = eval t == eval t'

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
  return x = MaybeT $ return (Just x)
  m >>= f = MaybeT $ do
    mx <- runMaybeT m
    case mx of
      Nothing -> return Nothing
      Just x -> runMaybeT (f x)

instance MonadTrans MaybeT where
  lift m = MaybeT $ do
    a <- m
    return (Just a)