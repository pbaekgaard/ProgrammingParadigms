fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys
