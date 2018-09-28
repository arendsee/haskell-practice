instance Functor ((->) e) where
  -- fmap :: (a -> b) -> f a -> f b
  --         (a -> b) -> ((->) e) a -> ((->) e) b
  --         (a -> b) -> (e -> a) -> (e -> b)
  --         (a -> b) -> (e -> a) -> e -> b
  fmap :: (a -> b) -> (e -> a) -> e -> b
  fmap g f x = g (f x)
