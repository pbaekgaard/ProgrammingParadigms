-- Problem 1.1
rotate :: [a] -> [a]
rotate [] = []
rotate [a] = [a]
rotate (x:xs) = xs ++ [x]

-- The function is is parametric polymorphic. This is because the function specifies no parameter types.
-- Thus the input parameter can have any types. Ad Hoc would mean the function is less free about the type of the parameters. For example specifying that the values in the list has to be numbers. etc.

-- Problem 1.2
allrotates :: [a] -> [[a]]
allrotates [] = []
allrotates [a] = [[a]]
allrotates (x:xs) = take (length (x:xs)) innerRotate
            where
                innerRotate = rotate [(x:xs)] ++ allrotates (rotate (x:xs))
-- The function is once again parametric polymorphic, by the same argumentation as the Problem 1.1

-- Problem 1.3
allrotates' :: [a] -> [[a]]
allrotates' [] = []
allrotates' [a] = [[a]]
allrotates' xs = map (\x -> drop x xs ++ take x xs) [0 .. length xs -1]


-- Problem 2.1
data Tree a = UnlabelledNode (Tree a) (Tree a) | Leaf a | LabelledNode a (Tree a) (Tree a) deriving Show

t1 :: Tree Int
t1 = UnlabelledNode (UnlabelledNode (Leaf 17) (Leaf 484000)) (Leaf 1964)

t2 :: Tree String
t2 = LabelledNode "bingo" (Leaf "plip") (LabelledNode "plop" (Leaf "uhu") (Leaf "fedtmule"))

-- Problem 2.2
isfull :: Tree a -> Bool
isfull (UnlabelledNode lChild rChild) = False 
isfull (Leaf a) = True
isfull (LabelledNode label lChild rChild) = True && isfull lChild && isfull rChild

-- Problem 2.3
preorder :: Tree a -> Maybe [a]
preorder (UnlabelledNode lChild rChild) = Nothing
preorder (Leaf a) = Just [a]
preorder (LabelledNode label lChild rChild) = do
                                                lLabel <- preorder lChild
                                                rLabel <- preorder rChild
                                                let thisLabel = [label]
                                                return (thisLabel ++ lLabel ++ rLabel)

-- Problem 3.1
remove :: String -> String -> String
remove string1 string2 = [x | x <- string1, all (\y -> y /= x) string2] 

-- Problem 3.2
removeRec :: String -> String -> String
removeRec [] string2 = []
removeRec (char:rest) string2 | all (\y -> y /= char) string2 = [char] ++ (removeRec rest string2)
                              | otherwise = removeRec rest string2


-- Problem 4.1
newtype WrapString a = WS(a, String) deriving Show

instance Functor WrapString where
    fmap f(WS(x,s)) = WS(f x,s)

instance Applicative WrapString where
    pure :: a -> WrapString a
    pure x = WS(x, "plip")
    (<*>) :: WrapString (a->b) -> WrapString a -> WrapString b
    WS(f, string1) <*> WS(a, string2) = WS(f a, string1 ++ string2)

instance Monad WrapString where
   return = pure 
   WS(x, string) >>= f = f x

-- pairup :: Monad m => WrapString a -> WrapString b -> m (WrapString (a,b))
pairup x y = do
        xx <- x
        yy <- y
        return ((xx,yy))

-- Problem 5.1
-- minMax :: (Ord a, Num a) => a -> a -> a -> (a,a)
minMax a b c = (minimum [a+1-1,b,c], maximum [a,b,c])

-- Problem 5.2
-- keyValThingy :: [(Integer, p -> Char)]
keyValThingy = [(1, (\x -> 'p'))]

-- Problem 5.3
prob3 :: (t1 -> Bool -> t2) -> t1 -> t2
prob3 f x = f x True

-- Problem 5.4
-- :t [1..4] = (Num a, Enum a) => [a]
-- The reason we haven't defined a function. is because then we would get [Integer]
-- The function cant have (Num a, Enum a) => [a]. But the expression (the right side of the = of a function) can.

-- Problem 6.1

ones = 1 : ones

naturals = 1 : zipWith (+) naturals ones

-- Problem 6.2
facs = map (\x -> product [1..x]) (0:naturals)

-- Problem 6.3
facs' = 1 : zipWith (*) naturals facs'
