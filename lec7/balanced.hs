data Tree a = Leaf a | DualNode a (Tree a) (Tree a) | SingleNode a (Tree a)

balanced (Leaf a) = True
balanced (DualNode val l r) = (numLeafs l == numLeafs r || numLeafs l == numLeafs r + 1 || numLeafs l + 1== numLeafs r) && balanced l && balanced r
                        where numLeafs (Leaf a) = 1
                              numLeafs (DualNode val l r) = numLeafs l + numLeafs r
                              numLeafs (SingleNode val m) = numLeafs m

balanced (SingleNode val m ) = True

balancedTree = DualNode "root" (Leaf "te") (Leaf "t")
balancedTree2 = DualNode "root" (DualNode "node" (Leaf "t") (Leaf "2")) (Leaf "t")

balancedTree3 = DualNode "root" (DualNode "node" (SingleNode "s2notes" (Leaf "leaft"))(Leaf "Leafy")) (SingleNode "snote" (Leaf "leaf"))
