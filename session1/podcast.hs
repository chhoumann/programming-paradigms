-- double x = x + x
-- quadruple x = double (double x)

factorial n = product [1..n]
average ns = div (sum ns) (length ns) -- or sum ns `div` length ns