-- A silly add function that fails for values greater than 100
boundAdd :: Int -> Int -> Maybe Int
boundAdd x y
  | x + y < 100 = Just (x + y)
  | otherwise   = Nothing

main :: IO ()
main = do
  print $ do
    x1 <- 10 `boundAdd` 5
    x2 <- x1 `boundAdd` 10
    1000 `boundAdd` 10 -- this evaluates to Nothing, which
                       -- propagates, resulting in a final
                       -- return of Nothing, even x2 would
                       -- be a `Just 25` otherwise.
    return x2
