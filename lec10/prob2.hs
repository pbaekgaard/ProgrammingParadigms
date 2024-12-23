import qualified Prelude
import Prelude hiding (Functor, fmap)

-- Define Functor type class (if not using the default one)
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Functor instance for ((->) r)
instance Functor ((->) r) where
    -- fmap :: (a -> b) -> ((->)r) a -> ((->)r) b
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g
