sumrows :: Num a => [[a]] -> [a]
sumrows xs = map (foldr (+) 0) xs
