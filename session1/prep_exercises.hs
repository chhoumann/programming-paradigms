-- Exercise 2
-- Define a function that will, when given a list, return the second element of the list if it exists.
-- second [1,2,5,6] gives 4.
-- second ["some", "bizarre", "mango"] gives "bizarre".

second [] = undefined
second (x:l) = head l