twice :: (f -> f) -> f -> f
twice f x = f(f x)

dingo (x, y) = [x, y]

mango x y z = x * y + z - 42

bingo a = a

bighead :: (Num a, Ord a) => [a] -> Int
bighead xs = count
    where   h = head xs
            count = length [e | e <- tail xs, e > h]


-- Exercise F
mapp :: (i -> o) -> [i] -> [o]
mapp f xs = [f x | x <- xs]

double n = 2*n