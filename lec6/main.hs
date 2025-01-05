-- Exercise 1.
dbs :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
dbs xs | null xs = []
       | otherwise = filter (\(x,y) -> x * 2 == y ) xs

-- Exercise 2.
within :: (Num a, Ord a) => [a] -> (a,a) -> [a]
within xs (minVal, maxVal) | null xs = []
                           | otherwise = filter (\x -> x >= minVal && x <= maxVal) xs

-- Exercise 3.
sumrows :: (Num a) => [[a]] -> [a]
sumrows xs | null xs = [0]
           | otherwise = map sum xs

-- Exercise 4.
fact :: Double -> Double
fact k = product [1..k]

approx :: Double -> Double
approx n = sum (map (\x -> 1 / fact x) [0..n])

-- Exercise 5.
--
-- Fingo is a function that takes two list as arguments and cons (prepends) the second list to the first list

-- EXTRA EXERCISES
-- Exercise a.

-- partition_filter :: (b -> Bool) -> [b]-> ([b], [b])
partition_filter p xs | null xs = ([], [])
                      | otherwise = (filter p xs, filter (not . p) xs)

partition_foldr p xs | null xs = ([], [])
                     | otherwise = foldr (\x (ts, fs) -> if p x then (x:ts, fs) else (ts, x:fs)) ([], []) xs

-- Exercise b.
filter_using_foldr p xs | null xs = []
                        | otherwise = foldr (\x ts -> if p x then x:ts else ts) [] xs

-- Exercise c.
-- What is the type of map map?
-- map is a function that takes a function and a list as arguments. It applies the function to each element in the list. So map map would have the following type:
-- map map :: [a -> b] -> [[a] -> [b]]

-- min2 xs | null xs = 0
--         | length xs == 1 = 0
--         | otherwise foldr (\x y -> if x >= min xs && (x <= y || y == 0) then x else y) 0 xs
