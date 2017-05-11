import Data.Monoid
import Data.Foldable

type ID = Integer

data Tree a = Nil | Node ID a [Tree a] deriving(Show)

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
  let c = Node 3 "yo" []
  let d = Node 4 "bu" [a,b]
  let e = Node 5 "do" [a,b,c]
  print $ e <> d <> c
