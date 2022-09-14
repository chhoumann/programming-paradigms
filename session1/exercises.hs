allButSecond [] = undefined
allButSecond (x:l) = [x] ++ tail l

fruits = ["banana", "orange", "mango"]

midtover [] = undefined
midtover as = (take half as, drop half as)
    where half = length as `div` 2


bingo x y = mod x z
    where z = y + 42



qsort :: (Ord a) => [a] -> [a]

-- Descending order
qsort [] = []
qsort (x:xs) = big ++ [x] ++ small
                 where small = qsort [a | a <- xs, a <= x] -- sort a list of elements from xs that are <= than x
                       big   = qsort [a | a <- xs, a > x] -- sort a list of elements from xs that are > x

-- If we remove the equality check in small 


lastEl [a] = head ( reverse a )