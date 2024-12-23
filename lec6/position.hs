position :: [Char] -> [Int]
position [] = []
position xs = map (\x -> fromEnum x - 06) xs
