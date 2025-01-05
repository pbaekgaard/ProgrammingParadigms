import qualified Prelude
import Prelude hiding (Functor, fmap, (<*>), Applicative, pure)

-- Define Functor type class (if not using the default one)
class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- Problem 1.
-- We define the datatype UTree
data UTree a = Node a [UTree a] deriving Show

-- We create the Functor instance. Here we make it so that the function that is parsed is applied to the 'a' value of the Node and we map over all the UTree is the array. NOTE: Prelude.Functor was used because we hide them for other exercises.
instance Functor UTree where
    fmap f (Node a xs) = (Node (f a) (map (fmap f) xs))


-- Problem 2.
-- fmap is the function that takes a function and a type constructor or function as parameters.
-- fmap basically does this: fmap f g = call g and then call f on the result
--
-- The type of fmap be written as:
-- fmap :: (a -> b) -> f a -> f b
-- here f is the type constructor.
instance Functor ((->) r) where
    -- fmap :: (a -> b) -> ((->)r) a -> ((->)r) b
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g
    -- The (.) operator is function composition. It combines two functions into a single function calling the right most function first and then the left function on the result. thus giving us (a->b) -> (c -> a) -> (c -> b)


-- Problem 3.
-- For this exercise we need to make [] a Functor as we are hiding it from previous exercises.
instance Functor [] where
    fmap f xs = map f xs

instance Applicative [] where
   -- Pure is the function that converts a value into the same context as the applicative.
   -- e.g. converting an Int into a list of Ints with that value in it.
   pure a = [a] 
   -- Applicative Apply a.k.a FunnyStar takes a function in the same context, and applies that to a value in the context.
   -- In this case the context is a list. So we take a list of functions and apply that to a list of values.
   -- Making it recursive we can use fmap to call each function on all values and then call funny star of the rest of the functions and the xs.
   [] <*> xs = []
   fs <*> [] = []
   (f:fs) <*> xs = fmap f xs ++ (fs <*> xs)

-- Problem 4.
prodthree :: (Num a) => [a] -> [a] -> [a] -> [a]
prodthree xs ys zs = pure (\x y z -> x * y * z) <*> xs <*> ys <*> zs

