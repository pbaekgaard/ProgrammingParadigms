isolate :: Ord a => [a] -> a -> ([a], [a])
isolate [] x = ([],[])
isolate (element:list) x = if element == x then (l1, element:l2) else (element:l1, l2)
                           where (l1, l2) = isolate list x
