data Cached a b =
    Empty (a -> b)   |
    Full  (a -> b) b

instance Show (Cached a b) where
    show (Empty _  ) = "empty"
    show (Full  _ b) = "full"

evaluate' :: a -> Cached a b -> b
evaluate' a (Empty f  ) = f a
evaluate' _ (Full  f b) = b

run' :: Cached a b -> a -> Cached a b
run' (Empty f  ) a = Full f (f a)
run' (Full  f b) _ = Full f b


--------------------------------------------------------------------------
pipe' :: Cached a b -> Cached b c -> Cached a c
-- for two empty containers, just pipe the functions
pipe' (Empty f) (Empty g) = Empty (g . f)
-- if the former is a cached, get a closure
pipe' (Full f x) (Empty g) = Full (g . f) (g x)
-- if both are full, then return the cached composition
pipe' (Full f x) (Full g y) = Full (g . f) y
-- what to do in this situation is a design decision, there are several
-- reasonable choices, I choose the safe route 
pipe' (Empty f) (Full g y) = Empty (g . f)
-- and infix:
a |. b = a `pipe'` b

getF :: Cached a b -> a
getF (Empty  f  ) = f
getF (Cached f _) = f

--------------------------------------------------------------------------
compose2 :: (a -> b) -> (b -> c) -> (b -> d) -> (a -> ((a -> c), (a -> d)))
compose2 f g h = \x -> ((g . f) x, (h . f) x)

branch' :: Cached a b -> Cached b c -> Cached b d -> Cached a (c,d)
branch' (Empty f  ) c1 c2 = Empty (compose2 f g h) where
    g = getF c1
    h = getF c2
branch' (Full  f x) c1 c2 = Full  (compose2 f g h) (g', h') where
    g  = getF c1
    h  = getF c2
    g' = run' c1 x
    h' = run' c2 x

--------------------------------------------------------------------------
merge' :: (b -> d -> e) -> Cached a b -> Cached c d -> Cached (b,d) e
merge' f (Empty g  ) (Empty h  ) = Empty (\x,y -> f (g x) (h y))
merge' f (Full  g x) (Empty h  ) = Empty (\y   -> f x     (h y))
merge' f (Full  g x) (Full  h y) = Empty (f x y)


--------------------------------------------------------------------------

main = do

    let s1 = "this is not a cat"
    let s2 = "methinks it be a dog"

    -- load a function into a Cache structure
    let x = Empty words
    -- run the function on input, caching the result
    let y = run' x s1
    -- this will run the function on the given input
    print $ evaluate' s2 x
    -- this will get the cached value
    print $ evaluate' s2 y

    let cwords   = Empty words
    let creverse = Empty reverse
    let cunwords = Empty unwords

    -- standard composition
    print $ (unwords . reverse . words) s2 
    -- pipe composition of wrapped functions
    print $ (pipe' cwords (pipe' creverse cunwords))
    -- evaluate wrapped composition
    print $ evaluate' s2 (pipe' cwords (pipe' creverse cunwords))
    -- evaluate wrapped composition with infix notation
    print $ evaluate' s2 (cwords |. creverse |. cunwords)
