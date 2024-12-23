sumsq n = foldr((+) . (^2)) 0 [1..n]
