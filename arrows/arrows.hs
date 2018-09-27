-- from: https://www.haskell.org/arrows/

class Arrow a where
  -- Each function may be treated as a computation.
  arr :: (b -> c) -> a b c

  -- Computations may be composed, by connecting the output of the first to the input of the second.
  (>>>) :: a b c -> a c d -> a b d

  -- A computation may be applied to part of the input, with the rest copied through to the output.
  first :: a b c -> a (b,d) (c,d)
