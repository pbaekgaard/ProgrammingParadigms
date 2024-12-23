
plonk :: Int -> (Int -> (Int -> Int))
plonk = \x -> (\y -> (\z -> x + y + z))
