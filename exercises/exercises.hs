-- Q1: what is the value of maybeAdd?

boundAdd :: Int -> Int -> Maybe Int
boundAdd x y
  | x + y < 100 = Just (x + y)
  | otherwise   = Nothing

maybeAdd :: Maybe Int
maybeAdd = do
  x1 <- boundAdd 5 10
  x2 <- boundAdd x1 10
  boundAdd 1000 10
  return x2

-- Q2: translate maybeAdd into an expression that does not use do notation
-- Solution:
--   boundAdd 5 10 >>= \x1 -> ( boundAdd x1 10 >>= \x2 -> ( boundAdd 1000 10 >> return x2 ) )

-- Q3: what does this return
boundAdd 5 10 >>= \x1 -> ( boundAdd x1 10 >>= \x2 -> ( boundAdd x1 x2 >> return x2 ) )
