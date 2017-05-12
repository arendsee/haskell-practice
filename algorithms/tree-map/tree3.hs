import Data.Monoid
import Data.Functor
import Data.Foldable

type ID = Integer

data Tree a = Nil | Node ID a [Tree a] deriving(Show)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node i x xs) = Node i (f x) (fmap (fmap f) xs)

instance Eq (Tree a) where
  (Node i _ _) == (Node j _ _) = i == j
  Nil == Nil = True

instance (Monoid a) => Monoid (Tree a) where
  mempty = Nil
  mappend (Node i a as) (Node j b bs) =
    Node i (mappend a b) (as ++ (filter (not . (flip elem) as) bs))
  mappend x Nil = x
  mappend Nil x = x

main = do
  let a = Node 1 "hi" []
  let b = Node 2 "bi" []
  let c = Node 3 "fo" []
  let d = Node 4 "bo" [a,b]
  let e = Node 5 "to" [a,b,c]
  let z = e <> d <> c 

  print z

  print $ fmap reverse z
