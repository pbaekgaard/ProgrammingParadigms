idhead :: Eq a => [(a,a)] -> Bool
idhead ((x,y):rest) = x==y
