data Encyclopedia a = Leaf String a | Node String a [Encyclopedia a] deriving Show

testEnc = Node "Mango" True [Node "Dingo" False [Leaf "plip" True, Leaf "ninka" False], Leaf "plop" True, Node "plys" False [Leaf "boing" True]]


testEnc2 = Node "plonk" 1 [Node "Zap" 3 [Leaf "ninka" 8], Node "uhu" 4 [Leaf "gif" 9], Leaf "bingo" 5]

