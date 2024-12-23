data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where
    fmap g (Node x []) = Node (g x) []
    fmap g (Node x ts) = Node (g x) [fmap g t | t <- ts]


-- Example UTree
exampleTree :: UTree Int
exampleTree = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]
