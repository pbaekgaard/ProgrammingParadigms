letters = do
    w <- getLine
    printChar w
    where
        printChar [] = return ()
        printChar [x] = putChar x >> putStrLn ""
        printChar (x:xs) = do
                        putChar x
                        putStrLn ""
                        printChar xs
