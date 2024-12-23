-- (<*>) :: [ a -> b] -> [a] -> [b]
-- gs <*> xs = [g x | g <- g, x <- xs]
import Prelude hiding (Applicative, (<*>))

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where
    pure x = [x]
    (f:fs) <*> xs = fmap f xs ++ (fs <*> xs)


