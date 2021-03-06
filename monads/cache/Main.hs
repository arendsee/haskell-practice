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

getF :: Cached a b -> (a -> b)
getF (Empty f  ) = f
getF (Full  f _) = f

getV :: Cached a b -> a -> b
getV (Empty f  ) a = f a
getV (Full  f b) _ =   b


--------------------------------------------------------------------------
compose2 :: (a -> b) -> (b -> c) -> (b -> d) -> (a -> (c, d))
compose2 f g h = \x -> ((g . f) x , (h . f) x)

branch' :: Cached a b -> Cached b c -> Cached b d -> Cached a (c,d)
branch' (Empty f  ) c1 c2 = Empty (compose2 f g h) where
    g = getF c1
    h = getF c2
branch' (Full  f x) c1 c2 = Full  (compose2 f g h) (g', h') where
    g  = getF c1
    h  = getF c2
    g' = getV c1 x
    h' = getV c2 x

--------------------------------------------------------------------------
merge' :: Cached a b -> Cached c d -> Cached (a,c) (b,d)
merge' (Empty g  ) (Empty h  ) = Empty (\(a,b) -> (g a, h b))
merge' (Full  g x) (Empty h  ) = Empty (\(a,b) -> (x  , h b))
merge' (Full  g x) (Full  h y) = Full  (\(a,b) -> (x  , y  )) (x,y)

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
    let chead    = Empty (fmap head)
    let cconcat  = Empty (\(x,y) -> x ++ " " ++ y)
    let cid      = Empty id

    -- standard composition
    print $ (unwords . reverse . words) s2 
    -- pipe composition of wrapped functions
    print $ (pipe' cwords (pipe' creverse cunwords))
    -- evaluate wrapped composition
    print $ evaluate' s2 (pipe' cwords (pipe' creverse cunwords))
    -- evaluate wrapped composition with infix notation
    print $ evaluate' s2 (cwords |. creverse |. cunwords)

    -- b1 :: String -> String
    let b1   = cwords |. creverse |. cunwords
    -- b2 :: String -> String
    let b2   = (branch' (cwords |. chead) cid creverse) |. cconcat
    -- prog :: (String, String) -> String
    let prog = (merge' b1 b2 |. cconcat)
    print $ evaluate' (s1, s2) prog
