nub :: (Eq a) => [a] -> [a]
-- nub list = reverse (list' list [])
--   where
--     list' [] acc = acc
--     list' (x : xs) acc
--       | x `elem` acc = list' xs acc
--       | otherwise = list' xs (x : acc)
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

isAsc :: [Int] -> Bool
isAsc [_] = True
isAsc (x : y : xs)
  | x <= y = isAsc xs
  | otherwise = False
