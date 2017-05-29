{-# LANGUAGE ExistentialQuantification #-}

data E = forall a. Show a => E a

instance Show E where
  show (E x) = show x

foo :: Int -> Int
foo x = 2 * x

bar :: Int -> Maybe Int
bar x
  | x == 5    = Just x
  | otherwise = Nothing

baz :: Int -> Bool
baz x
  | x < 5     = True
  | otherwise = False

joinFields :: Int -> [Int -> E] -> String
joinFields i es = unlines $ map (show . ($ i)) es where

main :: IO ()
main = do
  putStr $ joinFields 5 [E . foo, E . bar, E . baz]
