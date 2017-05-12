import Data.Monoid
import Data.Functor
import Data.List
import Control.Monad

data Tree a = Nil | Node { value :: a, inputs :: [Tree a] } deriving(Show, Eq)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)

-- screw it, lets stop thinking about type classes, and think about the
-- functions I actually need.

-- Tree to list, just a list of all a
tree2list :: Eq a => Tree a -> [a]
tree2list Nil = []
tree2list (Node x xs) = union [x] (xs >>= tree2list)

-- modify parent by comparing to children
familyMap :: (a -> [a] -> b) -> Tree a -> Tree b
familyMap _ Nil = Nil
familyMap f (Node t ts) = Node new_val new_kids  where
  new_val = f t $ liftM value $ ts
  new_kids = map (familyMap f) ts

-- modify parents based only on children
childMap :: ([a] -> b) -> Tree a -> Tree b
childMap _ Nil = Nil
childMap f (Node _ ts) = Node new_val new_kids where
  new_val = f $ liftM value $ ts
  new_kids = map (childMap f) ts

main = do
  let a = Node "jes"   []
  let b = Node "jak"   []
  let c = Node "jen"   [a,b]
  let d = Node "gracy" [c]
  let e = Node "dave"  [d,b]

  print $ tree2list e

  print $ e
  print $ fmap reverse e

  print $ childMap length e
  print $ tree2list $ familyMap family_desc e where
    family_desc parent children = 
      parent ++ " has " ++ (show . length) children ++ " children"
