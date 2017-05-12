-- I want to figure out how user-defined infix operators, precedence and
-- associativity work in haskell.

infixl 5 @+@
(@+@) :: Integer -> Integer -> Integer
x @+@ y = x + y

infixl 6 @*@
(@*@) :: Integer -> Integer -> Integer
x @*@ y = x * y

infixr 7 @^@
(@^@) :: Integer -> Integer -> Integer
x @^@ y = x ^ y

infixr 4 @|@
(@|@) :: a -> b -> (a,b)
x @|@ y = (x, y)

main :: IO ()
main = do
  print $ 1 @+@ 2 @+@ 3 @+@ 4
  print $ 1 @+@ 2 @*@ 3 @^@ 2 @|@ 2 @^@ 10 @|@ 10 @+@ 2 @*@ 5
