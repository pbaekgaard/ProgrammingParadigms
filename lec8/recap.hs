
-- 1. Find Haskell expressions that have the types


---- 1. (Ord a, Num a)=>a −> a −> [[Bool]] −> Bool


typesFirst num1 num2 (bool:bools) = num1+2 > num2+2 || all (== True) (concat bools)

---- 2. Num a =>(t −> a, t)−> a −> a

typesSecond (func, t) num = func t + num

---- 3. Fractional t1 =>(t2 −> t1)−> (t2 −> t1)−> (t1 −> t3)−> t2 −> t3

typesThird func1 func2 func3 teetoo = func3 (func1 teetoo / func2 teetoo)

-- 2. SKIPPED

-- 3. InVector
class InVector a where
    (&&&) :: a -> a -> a
    (***) :: a -> a -> a

instance InVector Bool where
    True &&& _ = True
    False &&& True = False
    False &&& False = False
    True *** False = False
    False *** True = False
    True *** True = True


-- 4. Frequencies
-- The goal of this problem is to define a function frequencies that, given a string s,
-- creates a list of pairs [( x1,f1) ,....( xk,fk) ] such that if the character xi occurs a total number of
-- fi times throughout the list s, then the list of pairs will contain the pair (xi , fi ).
-- As an example of this,
-- f r e q u e n c i e s ” r e g n i n g e r ”
-- should return the list
-- [ ( ’ r ’ , 2 ) , ( ’ e ’ , 2 ) , ( ’ g ’ , 2 ) , ( ’ n ’ , 2 ) , ( ’ i ’ , 1 ) ]

---- 1. What should the type of the function be?
-- frequencies :: String -> [(Char, Int)]


---- 2. Use recursion to give a definition of frequencies.
frequencies :: String -> [(Char, Int)]
frequencies [] = []
frequencies (ch:str) = (ch, count) : frequencies (filter (/=ch) str)
    where count = 1 + length [x | x <- str, x == ch]  -- Count includes 1 for the current character

-- The type Encyclopedia is given by the definition
data Encyclopedia a = Node String a [Encyclopedia a]
-- An encyclopedia is layered if it holds that all values at the same level of the encyclopedia are larger
-- than the values in the levels above. As an example, t2 in Figure 1 is layered, since 8 and 9 at level
-- 3 are greater than the values 3, 4 and 5 at level 2 – which are greater than the value 1 at level 1.
--
-- Define a function layered that can tell us if an encyclopedia is layered. Hint: The higher-order
-- functions all and map are useful.

t1 = Node "mango" True [
        Node "dingo" False [
            Node "plip" True [],
            Node "ninka" False []
        ],
        Node "plop" True [],
        Node "plys" False [
            Node "boing" True []
        ]
    ]

t2 = Node "plonk" 1 [
        Node "zap" 2 [
            Node "ninka" 8 []
        ],
        Node "uhu" 3 [
            Node "gif" 4 []
        ],
        Node "bingo" 5 []
    ]

-- 4. Layered

layered (Node _ val []) = True
layered (Node _ val children) = all (> val) [val | (Node _ val children) <- children] && layered rooted
    where 
        rooted = Node "newRoot" maxVal allChildren
        maxVal = maximum [val | (Node _ val children) <- children]
        allChildren = concat [children | (Node _ val children) <- children]

