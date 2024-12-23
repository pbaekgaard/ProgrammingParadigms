wrapup :: Ord a => [a] -> [[a]]
wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:xs) = if x == y
                then (x:headList):restOfMainList
                else [x] : (headList:restOfMainList)
                where (headList:restOfMainList) = wrapup xs
                      (y:_) = headList
