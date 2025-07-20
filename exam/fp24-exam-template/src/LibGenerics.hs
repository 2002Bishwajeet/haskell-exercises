{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LibGenerics where

-- Primitive Functors ----------------------------------------------------------

infixr 5 :+:
infixr 6 :*:

newtype Const b a = Const b deriving (Show, Eq)

newtype Id a = Id a deriving (Show, Eq)

data (:+:) f g a = Inl (f a) | Inr (g a) deriving (Show, Eq)

data (:*:) f g a = Pair (f a) (g a) deriving (Show, Eq)

instance Functor (Const b) where
  fmap _ (Const x) = Const x

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Pair x y) = Pair (fmap f x) (fmap f y)

-- Functor Fixpoints -----------------------------------------------------------

newtype Fix f = Fix (f (Fix f))

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix fx) = "(Fix (" ++ show fx ++ "))"

instance Eq (f (Fix f)) => Eq (Fix f) where
  Fix x == Fix y = x == y

