lettersSeq = do
    w <- getLine
    printChars w
    where
        printChars stringInput = sequence_ [putChar char >> putStrLn "" | char <- stringInput]
