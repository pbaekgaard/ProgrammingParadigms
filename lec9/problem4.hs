hugorm :: IO ()
hugorm = do
    putStr "Enter the number of integers to sum: "
    n <- readLn :: IO Int  -- Read the number of integers to read
    numbers <- sequence [readLn :: IO Int | _ <- [1..n]]
    putStr "The sum is "
    print (sum numbers)
