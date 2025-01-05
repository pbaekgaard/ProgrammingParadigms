uncurriedAddition nums =
    let
            a = fst nums
            b = snd nums
    in a + b

addition = curry uncurriedAddition
addOne = addition 1

addThreeNums x y z = x + y + z
addThreeNumsUncurried = uncurry (uncurry addThreeNums
