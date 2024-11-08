import Test.QuickCheck

dollarRate2018 = 1.18215

dollarRate2019 = 1.3671

dollarRate2022 = 0.98546541

dollarRate = 0.91691801

-- Comment starts like this

-- | convert EUR to USD
usd euros = euros * dollarRate

-- | convert USD to EUR
euro usd = usd / dollarRate

prop_EuroUSD x = euro (usd x) ~== x

-- Nearly equal
x ~== y = x - y < 10e-15 -- Made a new operator | ~==

price = 79

price' :: Double
price' = 79

increment :: Int -> Int
increment x = x + 1

increment' :: Int -> Int
increment' = \x -> x + 1

{- In general Haskell takes only single parameter so there are two ways
   to pass multiple paramerts to a function-}

-- | Curried Functions
add :: Int -> Int -> Int -- What it means is that add takes an Int and returns a function that takes an Int and returns an Int
add x y = x + y

--  | Uncurried Functions
add' :: (Int, Int) -> Int -- What it means is that add' takes a tuple of Int and returns an Int
add' (x, y) = x + y

-- NOTE: Prefer curried functions over uncurried functions