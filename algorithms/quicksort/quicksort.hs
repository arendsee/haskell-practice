-- Roughly based off Miran Lipovaca's implementation

-- Verbose but clearer
q1 :: (Ord a) => [a] -> [a]
q1 [] = []
q1 (x:xs) =
    let small = q1 [a | a <- xs , a <= x]
        big   = q1 [a | a <- xs , a > x ]
    in small ++ [x] ++ big

-- Less verbose, but not tasteless
q2 :: (Ord a) => [a] -> [a]
q2 []=[]
q2 (x:r)=q2 [a|a<-r,a<=x] ++ [x] ++ q2 [a|a<-r,a>x]

-- Obviously trying to be smart (though still well under 80 chars)
q3 []=[]; q3 (x:r)=q3 [a|a<-r,a<=x] ++ [x] ++ q3 [a|a<-r,a>x]

main = do
    -- and all of them work
    print (q1 [1,5,43,6,7,234,5,1,3,3])
    print (q2 [1,5,43,6,7,234,5,1,3,3])
    print (q3 [1,5,43,6,7,234,5,1,3,3])
