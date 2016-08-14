fizzbuzz :: Int -> String
fizzbuzz x
  | mod x 15 == 0 = "fizzbuzz"
  | mod x 5  == 0 = "buzz"
  | mod x 3  == 0 = "fizz"
  | otherwise     = show x

main = do print (map fizzbuzz [1..100])
