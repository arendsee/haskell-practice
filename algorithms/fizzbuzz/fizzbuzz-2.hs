bmod :: Int -> Int -> Int
bmod a b
  | mod a b == 0 = 1
  | otherwise    = 0

ids :: Int -> Int
ids x = (bmod x 3) + (bmod x 5) * 2

classify :: Int -> String 
classify x = (show x : a) !! ids x

a = ["fizz", "buzz", "fizzbuzz"]
main = print (map classify [1..100])
