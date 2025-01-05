data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node left right) = Node (fmap f left) (fmap f right)
