pyt :: Int -> [(Int, Int ,Int)]
pyt k = [(a,b,c) | a <- [1..k], b <- [1..k], c<-[1..k], a^2+b^2==c^2, a <= b, b < c]

