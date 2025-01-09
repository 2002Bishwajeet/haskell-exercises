-- Exercise 1
f :: [Int] -> Int
f xs = sum (filter (> 0) (map (* 2) xs)) -- Do it without parenthesis

f1 :: [Int] -> Int
f1 xs = sum $ filter (> 0) $ map (* 2) xs

f2 :: [Int] -> Int
f2 xs = sum . filter (> 0) . map (* 2) $ xs

f3 :: [Int] -> Int
f3 = sum . filter (> 0) . map (* 2)

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

--  Lazy Evaluation

cycle' :: [Int] -> [Int]
cycle' xs = xs ++ cycle' xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)