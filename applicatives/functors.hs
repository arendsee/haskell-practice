class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Applicative m where
  pure :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b


instance Applicative [a] where 
  -- pure :: a -> m a
  -- pure :: a -> [a]
  pure x = [x]

  -- (<*>) :: m (a -> b) -> m a -> m b
  -- (<*>) :: [(a -> b)] -> [a] -> [b]
  fs (<*>) xs = concat $ map (\f -> map f xs) fs

main = getLine >>= readFile >>= putStrLn


add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

-- add3 <$> just 1 <*> Just 2 <*> Just 3
-- ((add3 <$> Just 1) <*> Just 2) <*> Just 3   -- <*> and <$> have equal
--                                             -- precedence and are left
--                                             -- associative

-- fmap add3 |-
--   (a -> b) -> m a -> m b
--   (Int -> Int -> Int -> Int) -> f Int -> f (Int -> Int -> Int)
