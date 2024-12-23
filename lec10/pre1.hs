data Onion a = Core a | Layer (Onion a)

instance Functor Onion where
    fmap f (Core a) = Core (f a)
    fmap f (Layer b) = Layer (fmap f b)
