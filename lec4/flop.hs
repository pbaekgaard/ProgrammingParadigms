flop :: [(x,y)] -> [(y,x)]
flop list = [(v,z) | (z,v) <- list]
