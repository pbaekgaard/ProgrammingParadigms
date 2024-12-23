rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
rev list = last list : rev (init list)
