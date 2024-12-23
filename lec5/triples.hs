triples :: [(a,a,a)] -> ([a], [a], [a])
triples [] = ([],[],[])
triples [(a,b,c)] = ([a], [b], [c])
triples ((a,b,c):tupes) = (a : x, b : y, c : z)
                where (x,y,z) = triples tupes

