data Aexp = IntExp Int | StrExp String | Add Aexp Aexp | Mul Aexp Aexp

eval :: Aexp -> (String -> Int)-> Int
eval (IntExp e1) ass = e1
eval (StrExp e1) ass = ass e1
eval (Add e1 e2) ass = eval e1 ass + eval e2 ass
eval (Mul e1 e2) ass = eval e1 ass * eval e2 ass

myAss "x" = 3
myAss "y" = 4
