pascal :: Integer -> [Integer]
pascal 1 = [1]
pascal a = [head p] ++ (zipWith (+) (init p) (tail p)) ++ [last p]
    where
        p = (pascal (a-1))

main = do
    print (pascal 1)
    print (pascal 2)
    print (pascal 3)
    print (pascal 4)
    print (pascal 5)
