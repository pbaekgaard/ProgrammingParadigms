fibsfrom n1 n2 = (n1 + n2)  : fibsfrom n2 (n1 + n2)


-- allBinaries :: [String]
allBinaries :: [String]
allBinaries = concatMap generate [1..]
  where
    generate 1 = ["0", "1"]
    generate n = [b : bs | bs <- generate (n - 1), b <- "01", (b == '1' || head bs == '1') && last bs /='0']
