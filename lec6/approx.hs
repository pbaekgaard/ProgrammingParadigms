fact :: (Num a, Enum a, Fractional a) => a -> a
fact k = product [1..k]

approx :: Float -> Float
approx n = foldr ((+) . (1 /) . fact) 0 [0..n]
