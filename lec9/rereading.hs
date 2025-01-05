
-- Problem 1
-- The function takes a integer as input from the getLine function. Thus it is an IO operation.
-- It the reads the line and inputs that into the loop function. The loop function will print 1 if the line is == 1.
-- If the line is != 1, then it writes that line and then recursively calls loop with half the value or 3x+1 the value depending on if the value is even or uneven.

-- Problem 2.
-- This is an interactive function. It first looks for a user input (a string). Which is parsed to the putLetters function
-- the putLetters function prints the first char followed by a newline and then recursively calls it self with the rest
-- of the string.
letter = do
         str <- getLine
         putLetters str
         where
            putLetters [] = return ()
            putLetters (x:xs) = do 
                             putChar x
                             putChar '\n'
                             putLetters xs

-- Problem 3.
-- The function is also interactive.
-- Again we get a userinput (string)
-- sequence_ expects an array of monadic actions, i.e. IO operations (IO is a monad).
-- So we use list comprehension to generate a list of IO operations.
-- In this case we make a list of putStrLn which prints each character from the string and a newline
letters = do
          str <- getLine
          sequence_ [putStrLn [x] | x <- str]

-- Problem 4.
-- The function again is an interactive function due to the IO()
-- It starts by reading a line (we expect it to be an Int, as we try to parse it as an Int on the next line.
-- We use the number it a list comprehesion which creates  a list of fmap's that does getLine and parse that line into an Int
-- we then sum the numbers and print it.
hugorm :: IO()
hugorm = do
    w <- getLine
    let num = read w :: Int
    numbers <- sequence [(read :: String -> Int) <$> getLine | _ <- [1..num]]
    putStr $ "Sum: " ++ show (sum numbers) ++ ['\n']
