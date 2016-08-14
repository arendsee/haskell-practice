-- defining a list

let a = [1,2,3]
let a = [1 .. 10]

-- accessing elements
a !! 4
a !! 0

let b = [[1,2,3], [34,5], []]

head a
tail a
last a
init a
length a
null a
reverse a
take 3 a
drop 3 a
maximum a
minimum a
product a
-- 5 in a?
elem 5 a

[1..10]
[1,3..11]
[1,3..12]
['a','d'..'z']

-- infinite list
[1,7..]

take 5 (cycle [1,2])

take 10 (repeat 1)

replicate 10 1


-- list comprehensions

foo = [x^2 | x <- [1..], mod x 11 == 7]
take 5 foo

fizzbuzz x = if mod x 15 == 0 then "fizzbuzz" else if mod x 3 == 0 then "fizz" else if mod x 5 == 0 "buzz" else show x
map fizzbuzz [1..100]

filter odd [1..10]
map odd [1..10]
