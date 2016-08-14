-- get the next number
succ 1

-- get the next character
succ 'b'

-- get the max of two things
-- breaks on strings
max 45 3

-- functons have highest precedence

succ 1 + max 45 2 + 1

-- integer division
-- prefix form
div 13 2
-- infix form
13 `div` 2

-- all builtin functions:

-- math
1 + 2
2 * 4
2 ^ 4 -- exponentiation
3 / 2 -- real division
div 3 2 -- integer division
truncate 2.6
round 2.6
sqrt 23.5
gcd 14 21 -- greatest common denominator

[1,3] ++ [7,6] -- concatentate

-- boolean
1 < 2
1 > 2

not True -- negation

succ 4 -- next
succ 'a' -- nect character (successor)

-- IO
putStr "asdf"
putStrLn "asdf"
print 34

-- Read input (doesn't work interactively)
-- a <- readLn

-- mixing functions
do { putStr "2 + 2 = " ; print (2 + 2) }

-- type declaration
-- types are required to be upper case

x = 5 :: Int
x = 5 :: Double
x = 5 :: Float
x = 5 :: Rational

-- detect type
:t x

x = 5234239847293870419283749387 :: Integer
x ^ 100

-- : appends a value
1 : [1,2,3]

-- tuples can have different values
("asdf", 923874923874938749384783473847383874, 'c', 4.5)

-- zip - doesn't work with tuples
zip [1,2,3] ['c', 'r', 't']

-- list operations
[1 .. 10]
filter (>5) [1..10]
map (^2) [1..10]

-- first and second elements (only on ordered pairs)
fst (4,5)
snd (4,5)

map fst (zip [1,2,3] ['c', 'r', 't'])
map snd (zip [1,2,3] ['c', 'r', 't'])

-- for some reason this stalls and calls stack overflow
-- I'm probably missing the condition, I though it would stop when it
-- reached the set cases, but apparently not.
fibanocci n = if n > 2 then fibanocci (n-1) + fibanocci (n-2) else 1

-- if else - else is not optional in Haskell, something must be returned

-- "'" is valid, can be used for modified functions or non-lazy functions
a's = "sdf"
