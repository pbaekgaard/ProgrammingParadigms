
within :: (Num a, Ord a) => [a] -> (a,a) -> [a]
within xs tube = filter(\x -> x >= fst tube && x <= snd tube) xs
