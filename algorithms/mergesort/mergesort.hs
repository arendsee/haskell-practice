-- Merge two sorted arrays into one sorted array
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a:ar) (b:br)
    | a <= b    = [a] ++ ( merge ar    (b:br) )
    | otherwise = [b] ++ ( merge (a:ar)  br   )

msort :: (Ord a) => [a] -> [a]
msort []       = []
msort (x:[])   = [x]
msort (x:y:[]) = [min x y, max x y]
msort a = merge (msort (take i a)) (msort (drop i a))
    where i = div (length a) 2

main = do
    print (merge [1,6,7,8] [3,7,12])
    print (msort [1,5,34,6,234,7,4,56,3,123,23])
