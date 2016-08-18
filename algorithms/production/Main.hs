foo :: [Char] -> [Char]
foo [] = []
foo (x:r)
    | x == 'a'  = "ab"   ++ (foo r)
    | x == 'b'  = "a"  ++ (foo r)
    | otherwise =         (foo r)

-- take an argument and do it n times
foodo :: Int -> (a -> a) -> a -> a
foodo 0 f x = x
foodo 1 f x = f x
foodo n f x = f ( foodo (n-1) f x )

-- alternatively, could use foldr
foodo' :: Int -> (a -> a) -> a -> a
foodo' n f = ( foldr (.) f (replicate (n-1) f) ) 

main = do
    -- inefficient solution, since requires re-computing values
    print $ (foodo 0 foo) "aaa"
    print $ (foodo 1 foo) "aaa"
    print $ (foodo 2 foo) "aaa"
    print $ (foodo 3 foo) "aaa"

    print $ (foodo' 0 foo) "aaa"
    print $ (foodo' 1 foo) "aaa"
    print $ (foodo' 2 foo) "aaa"
    print $ (foodo' 3 foo) "aaa"
