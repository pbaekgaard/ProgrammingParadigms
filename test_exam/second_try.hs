-- Problem 1.1
rotate :: [a] -> [a]
rotate [] = []
rotate [a] = [a]
rotate xs = drop 1 xs ++ take 1 xs

-- The function is parametric polymorphic. This is because one or more of the variables in the function is generic (a)

-- Problem 1.2
allrotates :: [a] -> [[a]]
allrotates [] = [[]]
allrotates [a] = [[a]]
allrotates xs = take (length xs) ([xs] ++ rotations xs)
                where 
                    rotations :: [a] -> [[a]]
                    rotations xs = [rotate xs] ++ rotations (rotate xs)

-- We declare a function that creates the infinite list of rotations.
-- We then through lazy evaluation get the 'x' number of rotations including the base default state through the take (length xs).

-- Problem 1.3
allrotates' :: [a] -> [[a]]
allrotates' [] = [[]]
allrotates' [a] = [[a]]
allrotates' xs = map (\n -> iterate rotate xs !! n) [0..length xs - 1]
-- The solution uses iterate which infinitely calls rotate on it self. we use (!!) operator to take the n'th index of the infinite list.

-- Problem 2.1
data Tree a = NodeLabelled a (Tree a, Tree a) | Node (Tree a, Tree a) | Leaf a deriving Show

t1 = Node (Node (Leaf 17, Leaf 484000), Leaf 1964)
t2 = NodeLabelled "bingo" (Leaf "plip", NodeLabelled "plop" (Leaf "uhu", Leaf "fedtmule"))

-- Problem 2.2
isfull :: Tree a -> Bool
isfull (Leaf a) = True
isfull (Node (leftChild, rightChild)) = False
isfull (NodeLabelled _ (leftChild, rightChild)) = True && isfull leftChild && isfull rightChild
-- Here we set the base cases. That is leafs are labelled, and Node's are unlabelled. Thus if we find any Node's we should return False. Else we return True. NodeLabelled are also labelled so if we find one of these, we return True and then check the children of the NodeLabelled. We use && because we need to ensure that all children are labelled too.

-- Problem 2.3
preorder :: Tree a -> Maybe [a]
preorder (Leaf a) = Just [a]
preorder (Node (leftChild, rightChild)) = Nothing
preorder (NodeLabelled label (leftChild, rightChild)) = do
                lLabel <- preorder leftChild
                rLabel <- preorder rightChild
                return ([label] ++ lLabel ++ rLabel)
-- The function recursively calls it self. For the base case, the function returns the label of the leaf node. For Nodes we return nothing and we should go no further. For NodeLabelled we return the label and recursively call preorder on the children till we get a full list of labels.

-- Problem 3.1
remove :: String -> String -> String
remove "" rString = ""
remove lString "" = lString
remove lString rString = [x | x <- lString, all (\y -> y /= x) rString]
-- We make a list comprehension that assembles the leftString, and then checks that the x'th letter is not in the rString.

-- Problem 3.2
removeRec :: String -> String -> String
removeRec "" rString = ""
removeRec lString "" = lString
removeRec (x:lString) rString | (all (\y -> y /= x) rString) = [x] ++ removeRec lString rString
                              | otherwise = removeRec lString rString
-- Pretty much the same as in Problem 3.1 We check if all letters in the rightString is not equal to the current letter we are checking. If true we take the letter and then append the recursive call to the rest of the left string.

-- Problem 4.1
newtype WrapString a = WS(a, String) deriving Show

instance Functor WrapString where
    fmap f(WS(x,s)) = WS(f x,s)

instance Applicative WrapString where
    pure a = WS(a, "bingo")

    WS(f, fString) <*> WS(a, string) = WS(f a, string)

-- Problem 4.2
instance Monad WrapString where
    return = pure
    WS(a, string) >>= f = f a
-- we declare the return and >>= of WrapString


-- Problem 4.3
pairup :: WrapString a -> WrapString b -> WrapString (a,b)
pairup (WS(lVal, str)) r = do
            rVal <- r
            WS((lVal, rVal), str)
-- We extract the monadic values of the left and right and return a new WS with a = (lVal, rVal)

-- Problem 5.1
func1 a b c = (a, x)
            where x = if a > b then b + c else b + a

-- Problem 5.2
-- [(Integer, p -> Char)]
x = [(1, (\x -> 'c'))]

-- Problem 5.3
-- (t1 -> Bool -> t2) -> t1 -> t2
func func2 param = func2 param True

-- Problem 5.4
-- :t [1..3] returns
-- [1..3] :: (Num a, Enum a) => [a]

-- Problem 6.1
naturals :: [Integer]
naturals = generateNaturals 1
    where
        generateNaturals n = n : generateNaturals(n+1)
-- Using recursion we generate a list of n where n increases by 1 each time.

-- Problem 6.2
facs :: [Integer]
facs = map (\x -> product [1..x]) (0:naturals)

-- Problem 6.3
facs' :: [Integer]
facs' = [1] ++ zipWith (\x y -> x * y) naturals facs'
-- The way this works. Is that the zipWith multiplys the value at index n with its index and that will be the next value.
-- Because we do this on facs' recursively. the thing gets bigger and bigger. zipWith basically waits for the facs' to get updated and then continuesly makes it larger and larger.
