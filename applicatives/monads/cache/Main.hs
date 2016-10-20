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

compose' :: Cached a b -> Cached b c -> Cached a c
-- for two empty containers, just compose the functions
compose' (Empty f) (Empty g) = Empty (g . f)
-- if the former is a cached, get a closure
compose' (Full f x) (Empty g) = Full (g . f) (y x)
-- if both are full, then return the cached composition
compose' (Full f x) (Full g y) = Full (g . f) y
-- what to do in this situation is a design decision, there are several
-- reasonable choises, I choose the safe route 
compose' (Empty f) (Full g y) = Empty (g . f)

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

    let cwords   = Empty words
    let creverse = Empty reverse
    let cunwords = Empty unwords
    print $ evaluate compose' ((compose' cunwords creverse) cwords) "a cat"
