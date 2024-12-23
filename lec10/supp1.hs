data Exp a = Var a | Val Integer | Add (Exp a) (Exp a) | Mult (Exp a) (Exp a) deriving Show

instance Functor Exp where
    fmap f (Var a) = Var (f a)
    fmap f (Val x) = Val x
    fmap f (Add a b) = Add (fmap f a) (fmap f b)
    fmap f (Mult a b) = Mult (fmap f a) (fmap f b)


expr = Add (Var "x") (Mult (Val 5) (Var "y"))
