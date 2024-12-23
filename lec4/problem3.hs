bighead :: (Num a, Ord a) => [a] -> Int
bighead (xs:rest) = length [x | x <- rest, x > xs]
