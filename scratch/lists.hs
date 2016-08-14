import Data.List

count :: (Ord a) => [a] -> [(a, Int)]
count a = zip (nub a) (map length $ (group . sort) a)

run :: (Eq a) => [a] -> [(a, Int)]
run a = zip (map head $ g) (map length $ g)
    where
        g = group a

main = do
    let a = [1,3,3,6,4,4,4,3,4,3,3,3,3,5,6,6,6,6]
    print $ count a
    print $ run a
    print $ filter (\x -> snd x > 1) (run a)
