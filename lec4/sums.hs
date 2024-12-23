sums m n = concat [[x+y | x <- [1..n]] | y <- [1..m]]

sums' m n = [sum xs | xs <- sequence [[1..m],[1..n]]]

