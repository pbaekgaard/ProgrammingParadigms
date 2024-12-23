
-- Discussion 1
-- fourfirst xs = do
--      x <- xs
--      return (4, x)
--
-- What does the function do?
--
-- The function returns a list with tuples for each of the elements in x with the format of (4, 'element in xs')
-- E.g. if xs is the following list: [1,2,3]
-- we get a list with the tuple elements (4, 1), (4,2) and (4,3)

fourfirst xs = do
    x <- xs
    return (4, x)

data W x = Bingo x deriving Show

instance Functor W where
    fmap f (Bingo x) = Bingo (f x)

instance Monad W where
    return = pure
    Bingo x >>= f = f x

instance Applicative W where
    pure a = Bingo a
    (Bingo f) <*> (Bingo x) = Bingo (f x)


-- Discussion 2
wrapadd (Bingo x) (Bingo y) = do
     return (x+y)
--
-- Why is this is a bad way of using monads?
-- its because it does not use the monadic properties.
-- It will return a value of the type that is inside the monadic context. E.g. if Bingo x and Bingo y has int values. then it will just return an int and not a value in the monadic context Bingo.
-- 
wrapaddfixed ma mb = do
    x <- ma
    y <- mb
    return (x+y)


-- Problem 4 - Trees
data Tree a = Leaf a | Node (Tree a) (Tree a)

minmax :: (Ord a) => Tree a -> Maybe (a,a)
minmax (Leaf x) = Just (x, x)
minmax (Node left right) = do
    (x1, y1) <- minmax left
    (x2, y2) <- minmax right
    if y1 <= x2 then return (x1,y2)
    else Nothing

minorder t = do
    (min, max) <- minmax t
    return min

mytree1 = Node(Node(Leaf 1)(Leaf 2))(Leaf 2)
mytree2 = Node(Node(Leaf 3)(Leaf 2))(Leaf 1)


dingo :: Int -> IO Int
dingo x = do
    putStrLn (show x)
    return x

-- Correct foldM implementation
foldM :: Monad m => (a -> b -> m b) -> [a] -> b -> m b
foldM _ [] z = return z  -- Base case: no more elements, return the accumulator
foldM f (x:xs) z = do
    z' <- f x z   -- Apply the function to the head element and the accumulator
    return z
    foldM f xs z'  -- Recursively fold the rest of the list with the updated accumulator


-- Test case
test :: IO ()
test = do
    _ <- foldM (\x y -> dingo (x + y)) [1, 2, 3, 4] 0
    return ()
