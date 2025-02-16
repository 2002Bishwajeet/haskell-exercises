--  SemiGroup follows the law of associtiavity
-- It is particularly used with Monoid
class Semigroup' a where
  (<>) :: a -> a -> a

-- Monoid
--  a Monoid is a type class that extends the Semigroup type class with an identity element.
-- A Monoid must satisfy two main properties: associativity and identity.

{- class Semigroup' a => Monoid a where  -- Here we are defining the Monoid typeclass which extends SemiGroup'
    mempty' :: a
    mappend :: a -> a -> a
    mappend = (<>)
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty -}