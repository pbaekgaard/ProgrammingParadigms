onlytwo :: [a] -> Bool
onlytwo [_,_] = True
onlytwo _ = False

alldots :: Num a => [(a,a)] -> [(a,a)] -> [a]
alldots xs ys = [a*c+b*d | (a,b) <- xs, (c,d) <- ys]
