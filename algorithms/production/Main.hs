foo :: [Char] -> [Char]
foo [] = []
foo (x:r)
    | x == 'a'  = "ab"   ++ (foo r)
    | x == 'b'  = "a"  ++ (foo r)
    | otherwise =         (foo r)

foodo :: Int -> (a -> a) -> a -> a
foodo 0 f x = x
foodo 1 f x = f x
foodo n f x = f ( foodo (n-1) f x )

main = do
    print $ (foodo 0 foo) "aaa"
    print $ (foodo 1 foo) "aaa"
    print $ (foodo 2 foo) "aaa"
    print $ (foodo 3 foo) "aaa"
