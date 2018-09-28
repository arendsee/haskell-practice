class Arrow a where
  -- Raise a function into a Arrow context
  arr :: (b -> c) -> a b c

  -- Apply a computation to the first part of a value, passing the rest through unchanged
  first :: a b c -> a (b,d) (c,d)

  -- Generalized composition operator
  (>>>) :: a b c -> a c d -> a b d

  -- Parallel computation operator
  (***) :: a b c -> a b' c' -> a (b, b') (c, c')

  -- Fanout operator
  (&&&) :: a b c -> a b c' -> a b (c, c')


instance Arrow (->) where
  -- arr :: (b -> c) -> a b c
  -- arr :: (b -> c) -> (->) b c
  -- arr :: (b -> c) -> (b -> c)
  arr = id

  -- first :: a b c -> a (b,d) (c,d)
  -- first :: (b -> c) -> ((b,d) -> (c,d))
  -- first :: (b -> c) -> (b,d) -> (c,d)
  first f (x,y) = (f x, y) 

  -- (>>>) :: a b c -> a c d -> a b d
  -- (>>>) :: (b -> c) -> (c -> d) -> (b -> d)
  (>>>) :: flip (.)

  -- (***) :: a b c -> a b' c' -> a (b b') (c c')
  -- (***) :: (b -> c) -> (b' -> c') -> (b, b') -> (c, c')
  (***) f g (x, y) = (f x, g y)

  -- (&&&) :: a b c -> a b c' -> a b (c, c')
  -- (&&&) :: (b -> c) -> (b -> c') -> b -> (c, c')
  (&&&) f g x = (f x, g x)
