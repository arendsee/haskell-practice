import Data.Ord

data Tree a = Nil | Node (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
  show Nil = ""
  show (Node Nil c Nil) = show c
  show (Node l c r) = (show c) ++ "(" ++ (show l) ++ "," ++ (show r) ++ ")"

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node l c r) = Node (fmap f l) (f c) (fmap f r)

instance (Eq a) => Eq (Tree a) where
  Nil == Nil = True
  (Node l1 c1 r1) == (Node l2 c2 r2) = (l1 == l2) && (c1 == c2) && (r1 == r2) 
  _ == _ = False

instance (Ord a) => Ord (Tree a) where
  Nil <= Nil = True
  (Node l1 c1 r1) <= (Node l2 c2 r2) =
    (c1 < c2) ||
    ((c1 <= c2) && (l1 < l2)) ||
    ((c1 <= c2) && (l1 <= l2) && (r1 <= r2))

main = do
  let t1 = Node (Node (Node (Nil) 10 (Nil)) 20 (Nil)) 42 (Node (Nil) 1 (Nil))
  print t1
  print $ fmap (* 2) t1

  let t2 = Node (Node (Node (Nil) 10 (Nil)) 20 (Nil)) 42 (Node (Nil) 1 (Nil))

  let t3 = Node (Node (Node (Nil) 19 (Nil)) 20 (Nil)) 42 (Node (Nil) 1 (Nil))

  print $ t1 == t2

  print $ max t1 t3
