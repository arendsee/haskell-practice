import Data.Monoid
import Data.Functor

type ID = Integer

data Tree a = Nil | Node ID a [Tree a] deriving(Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node i x xs) = Node i (f x) (fmap (fmap f) xs)

instance Eq (Tree a) where
  (Node i _ _) == (Node j _ _) = i == j
  Nil == Nil = True

{-
 - I want an operator that links y to x
 -
 - x `mappend` y --> Node x [Node y]
 -
 - it should be right associative
 -
 - my first thought was just to redefine my previous monoid, however, the
 - result is not associative.
 -
 - Solution, there isn't one. The datastructure I want simply isn't a monoid,
 - but rather a member of the more general Applicative class.
--}

-- WARNING: This is an evil monoid, it is not associative
instance Monoid (Tree a) where
  mempty = Nil
  mappend x Nil = x
  mappend Nil x = x
  mappend (Node i a as) b =
    if any (== b) as then
      Node i a as
    else
      Node i a (as ++ [b])

main = do
  let a = Node 1 "a" []
  let b = Node 2 "b" []
  let c = Node 3 "c" []

  -- NOTE z1 != z2
  let z1 = ( a <> b ) <> c
  let z2 = a <> ( b <> c )
  let z3 = a <> b <> c

  print z1
  print z2
  print z3

  print $ fmap reverse z1
