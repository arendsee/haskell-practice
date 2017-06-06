import Control.Monad (guard)

-- A silly add function that fails for values greater than 100
boundAdd :: Int -> Int -> Maybe Int
boundAdd x y
  | x + y < 100 = Just (x + y)
  | otherwise   = Nothing

main :: IO ()
main = do
  -- inside a Maybe monad
  print $ do
    x1 <- 10 `boundAdd` 5
    x2 <- x1 `boundAdd` 10
    1000 `boundAdd` 10 -- this evaluates to Nothing, which
                       -- propagates, resulting in a final
                       -- return of Nothing, even x2 would
                       -- be a `Just 25` otherwise.
    return x2

  -- inside a list monad
  print $ do
    x1 <- [1,2,3]
    x2 <- ["Jes","Jac","Jen"]
    guard $ x1 /= 1
    return (x1,x2)

  -- Why does this do this? Basically, the list monad turns this into a nested
  -- for loop. Since the second value is ignored, this ends up just repeating
  -- the elements in the first vector.
  print $ do
    x1 <- [1,2,3]
    [222,45]
    return x1
