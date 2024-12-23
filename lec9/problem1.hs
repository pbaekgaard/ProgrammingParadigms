main = do
    w <- getLine
    loop ( (read w) :: Int)
    where
        loop 1 = putStr (show 1)
        loop x = do
            putStr (show x)
            if even x
                then loop (x `div` 2)
                else loop (3*x+1)

-- Explaination:
-- The main function reads the input and expects an integer value.
-- It then prints 1 if the integer is 1 or prints what ever the value is. Then it divides the value by 2 if it is even or multiplies it by 3 and adds 1 if its uneven. This new number is then input into the loop function which again prints 1 if its 1 or does the same as above.


