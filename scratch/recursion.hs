import System.Environment  

rprint :: Int -> [Int]
rprint 1 = [1]
rprint n = n:(rprint  (n - 1))

power_of_n :: Int -> Int -> Bool
power_of_n 1 _ = True
power_of_n n i = if (mod n i) == 0 then power_of_n (div n i) i else False 

head' :: [a] -> [a]
head' []       = []
head' (x:[])   = [x]
head' (x:y:_)  = [x,y]

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- broken for reasons unknown (to me)
binarySearch :: (Ord a) => a -> [a] -> Bool
binarySearch a x
    | (x !! i)     == a = True
    | (length x)   == 1 = False
    | (x !! i)      > a = binarySearch a (take (i-1) x)
    | (x !! i)      < a = binarySearch a (drop i x)
    where
        i = div (length x) 2 

main = do
    -- `read` - magic magic, do as you will
    print $ rprint 10
    print $ power_of_n (read "8") 2
    print $ head' [1]
    print $ head' [1,2,3,4,5]
    print $ fib 5
    print $ binarySearch 5 [1,2..10]
