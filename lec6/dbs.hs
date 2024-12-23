
dbs :: (Num a, Eq a) => [(a,a)] -> [(a,a)]
dbs [] = []
dbs xs = filter(\x -> fst x * 2 == snd x) xs
