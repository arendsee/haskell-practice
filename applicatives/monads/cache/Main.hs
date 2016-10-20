words' :: String -> [String]
words' = words

reverse' :: [a] -> [a]
reverse' = reverse

unwords' :: [String] -> String
unwords' = unwords

data Cached a b = Empty (a -> b) | Full (a -> b) b

evaluate' :: Cached a b -> a -> b
evaluate' (Empty f  ) a = f a
evaluate' (Full  f b) _ = b

run' :: Cached a b -> a -> Cached a b
run' (Empty f  ) a = Full f (f a)
run' (Full  f b) _ = Full f b

-- data Chain = Compound (a -> b) Chain | None

main = do
    -- load a function into a Cache structure
    let x = Empty words
    -- run the function on input, caching the result
    let y = run' x "not a cat"
    -- this will run the function on the given input
    print $ evaluate' x "a cat"
    -- this will get the cached value
    print $ evaluate' y "a cat"
