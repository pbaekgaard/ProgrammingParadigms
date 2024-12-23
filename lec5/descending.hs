descending :: Ord a => [a] -> Bool
descending [] = True
descending [a] = True
descending (first:second:rest) |  first >= second =  descending (second:rest)
                               | otherwise = False
