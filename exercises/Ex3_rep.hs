cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)