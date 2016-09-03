import Data.List

-- #1a
-- find the last element of a list
n1 :: [a] -> [a]
n1 [] = []
n1 x  = (take 1 . reverse) x

-- #1b
-- a solution that doesn't use other functions
n1' :: [a] -> Maybe a
n1' []     = Nothing
n1' (x:[]) = Just x
n1' (x:r)  = n1' r

-- #2
-- find the penultimate element of a list
n2 :: [a] -> [a]
n2 [] = []
n2 x  = (n1 . init) x

-- #2b
n2' :: [a] -> Maybe a
n2' []       = Nothing
n2' (_:[])   = Nothing
n2' (_:x:[]) = Just x
n2' (x:r)    = n2' r

-- #3
-- find element k
n3 :: Int -> [a] -> [a]
n3 k x  = (n1 . take k) x

-- #4
-- find number of elements in a list
n4 :: [a] -> Int
n4 []    = 0
n4 (x:r) = 1 + n4 r

-- #5
-- reverse a list
n5 :: [a] -> [a]
n5 [] = []
n5 (x:r) = n5 r ++ [x]

-- #6
-- determine if list is a palindrome
n6 :: Eq a => [a] -> Bool
n6 []     = False
n6 (x:[]) = True
n6 x      = (take k) x == (take k . reverse ) x
    where k = div (length x) 2

-- -- #7
-- -- flatten a nested list
-- -- I understand the problem, but I am familiar enough with types ...
-- data NestedList a = Elem a | List [NestedList a]
-- n7 :: NestedList a -> [a]
-- n7 (Elem x)   = [x]
-- n7 (List [])  = [ ]
-- n7 (List x:r) = n7 x ++ n7 r

-- #8
-- remove duplicated elements
n8 :: Eq a => [a] -> [a]
n8 [] = []
n8 (x:[]) = [x]
n8 (x:y:r)
    | x == y =        n8 (y:r)
    | x /= y = [x] ++ n8 (y:r)
-- -- here are a few alternative solutions from the wiki:
-- n8 [] = []
-- n8 (x:r) = x ++ (n8 $ dropWhile (== x) r)
--
-- -- super easy if we use libraries
-- import Data.List
-- n8 = map (head . group)

-- #9
-- merge repeated elements into sublists
n9 :: Eq a => [a] -> [[a]]
n9 = group  -- OK, this is cheating, I'll come back to it

-- #10
-- use #9 to get run length encoding
n10 :: Eq a => [a] -> [(Int, a)]
n10 [] = []
n10 x  = zip (map length (group x)) (n8 x)

main = do
    let x = [1,2..10]
    (putStrLn . show . n1) x 
    (putStrLn . show . n1') x 
    (putStrLn . show . n2) x 
    (putStrLn . show . n3 6) x 
    (putStrLn . show . n4) x 
    (putStrLn . show . n5) x 
    (putStrLn . show . n6) [3, 2, 1, 2, 3]
    (putStrLn . show . n8) [3,3,3,2,4,4,6,3,3]
    (putStrLn . show . n9) [3,3,3,2,4,4,6,3,3]
    (putStrLn . show . n10) "aaaadsddddddddrf" 
