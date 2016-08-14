map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:r) = f x : map' f r

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy a b = (mod b a) == 0

-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' _ [] = []
-- filter' p (x:r)
--     | p x       = x : filter' p r
--     | otherwise = filter' p r

-- Collatz sequences:
-- Let n be a natural number.
-- If n is even, divide it by two
-- If n is odd, multiply it by 3 and then add 1
-- Get an array of values ...

-- -- My first and quite flawed implementation
-- collatz' :: [Int] -> [Int]
-- collatz' (x:r)
--     | x == 1 = [x] ++ r
--     | even x = collatz' [div x 2]   ++ [x] ++ r 
--     | odd x  = collatz' [x * 3 + 1] ++ [x] ++ r
--
-- collatz :: Int -> [Int]
-- collatz x = reverse (collatz' [x])

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | even n = n:collatz(div n 2)
    | odd  n = n:collatz(n * 3 + 1)

discreteHalfLives :: Int -> [Int]
discreteHalfLives 0 = [0]
discreteHalfLives n = n:discreteHalfLives(div n 2)

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\x acc -> acc:x) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []

main = do
    -- all functions in Haskell formally take a single argument
    -- `max a b` technically is two functions
    -- `max a` and `(max a) b`
    print (map' (+2) [1,2,3])
    print (filter' (>5) [1..10])
    print (filter' (isDivisibleBy 5) [1..100])
    print (collatz 10)
    print (collatz 20)
    print (discreteHalfLives(3913487))
    print (maximum' [1,4,3,67,3])
    print (reverse' [1,4,3,67,3])
    print (product' [1..5])
    print (filter' (>5) [1,4,3,67,3,6])
