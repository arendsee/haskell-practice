{-# LANGUAGE ExistentialQuantification #-}

data Het = forall a. Show a => Het a
instance Show Het where
  show (Het a) = show a

data TwoHet = forall a b. (Show a, Show b) => Foo (a,b) | Bar Int
instance Show TwoHet where
  show (Foo (x,y)) = show (x,y)
  show (Bar x) = show x 

main :: IO ()
main = do
  let h = [Het "hi", Het 56, Het (Just 1)]
  putStrLn $ show h

  let b = [Foo (45,"er"), Bar 4, Foo (Just 4,"er")]
  putStrLn $ show b

  let c = [Foo (45,"er")] ++ [Bar 4, Foo (Just 4,"er")]
  putStrLn $ show c
